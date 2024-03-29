/**
 * @file eventManagerProxy.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/communicator.h"
#include "context.h"
#include "events/events.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"
#include "options/options.h"
#include "scheduler.h"

#include "messages.pb.h"

#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <tbb/concurrent_hash_map.h>

#include <atomic>
#include <chrono>
#include <memory>
#include <unordered_map>

#ifdef ENABLE_BACKWARD_CPP
#define BACKWARD_HAS_DW 1
#define BACKWARD_HAS_LIBUNWIND 1
#include <backward.hpp>
#endif

using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace one::communication;
using namespace boost::python;

class ReleaseGIL {
public:
    ReleaseGIL()
        : threadState{PyEval_SaveThread(), PyEval_RestoreThread}
    {
    }

private:
    std::unique_ptr<PyThreadState, decltype(&PyEval_RestoreThread)> threadState;
};

class ManagerProxy {
public:
    ManagerProxy(
        std::shared_ptr<Context<one::communication::Communicator>> context)
        : m_context{std::move(context)}
        , m_manager{*m_context->scheduler(), m_context->communicator(),
              std::chrono::seconds{30}}
    {
#ifdef ENABLE_BACKWARD_CPP
        backward::SignalHandling sh;
#endif
    }

    ~ManagerProxy() { stop(); }

    void emitFileRead(std::string fileUuid, off_t offset, size_t size)
    {
        m_manager.emit<FileRead>(std::move(fileUuid), offset, size);
    }

    void emitFileWritten(std::string fileUuid, off_t offset, size_t size)
    {
        m_manager.emit<FileWritten>(std::move(fileUuid), offset, size);
    }

    void emitFileTruncated(std::string fileUuid, off_t fileSize)
    {
        m_manager.emit<FileTruncated>(std::move(fileUuid), fileSize);
    }

    std::int64_t subscribeFileRead(
        std::int64_t counterThr, std::int64_t timeThr)
    {
        ReleaseGIL guard;
        return subscribeIOEvents<clproto::FileReadSubscription,
            FileReadSubscription>(counterThr, timeThr);
    }

    std::int64_t subscribeFileWritten(
        std::int64_t counterThr, std::int64_t timeThr)
    {
        ReleaseGIL guard;
        return subscribeIOEvents<clproto::FileWrittenSubscription,
            FileWrittenSubscription>(counterThr, timeThr);
    }

    std::int64_t subscribeFileAttrChanged(
        std::string fileUuid, std::int64_t timeThr)
    {
        ReleaseGIL guard;
        FileAttrChangedSubscription sub{std::move(fileUuid),
            std::chrono::milliseconds{timeThr},
            [this](auto) { this->incCounter("file_attr_changed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeFileLocationChanged(
        std::string fileUuid, std::int64_t timeThr)
    {
        ReleaseGIL guard;
        FileLocationChangedSubscription sub{std::move(fileUuid),
            std::chrono::milliseconds{timeThr},
            [this](auto) { this->incCounter("file_location_changed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeFilePermChanged(std::string fileUuid)
    {
        ReleaseGIL guard;
        FilePermChangedSubscription sub{std::move(fileUuid),
            [this](auto) { this->incCounter("file_perm_changed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeFileRenamed(std::string fileUuid)
    {
        ReleaseGIL guard;
        FileRenamedSubscription sub{std::move(fileUuid),
            [this](auto) { this->incCounter("file_renamed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeFileRemoved(std::string fileUuid)
    {
        ReleaseGIL guard;
        FileRemovedSubscription sub{std::move(fileUuid),
            [this](auto) { this->incCounter("file_removed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeQuotaExceeded()
    {
        ReleaseGIL guard;
        QuotaExceededSubscription sub{
            [this](auto) { this->incCounter("quota_exceeded"); }};
        return m_manager.subscribe(sub);
    }

    bool unsubscribe(std::int64_t subscriptionId)
    {
        return m_manager.unsubscribe(subscriptionId);
    }

    std::int64_t getHandlerCallsNumber(const std::string &handler)
    {
        decltype(m_counters)::const_accessor acc;
        if (m_counters.find(acc, handler)) {
            return acc->second;
        }
        return 0;
    }

    bool existsSubscription(std::int64_t subscriptionId, bool expected)
    {
        return m_manager.existsSubscription(subscriptionId) == expected;
    }

    void stop()
    {
        if (!m_stopped.test_and_set()) {
            m_context->communicator()->stop();
        }
    }

private:
    void incCounter(const std::string &handler)
    {
        decltype(m_counters)::accessor acc;
        if (m_counters.insert(acc, handler)) {
            acc->second = 1;
        }
        else {
            acc->second += 1;
        }
    }

    template <class ProtoSub, class Sub>
    std::int64_t subscribeIOEvents(
        std::int64_t counterThr, std::int64_t timeThr)
    {
        ProtoSub msg{};
        if (counterThr > 0)
            msg.set_counter_threshold(counterThr);
        if (timeThr > 0)
            msg.set_time_threshold(timeThr);

        return m_manager.subscribe(Sub{msg});
    }

    tbb::concurrent_hash_map<std::string, std::int64_t> m_counters;
    std::shared_ptr<Context<communication::Communicator>> m_context;
    Manager m_manager;

    using CounterAcc = typename decltype(m_counters)::accessor;
    using ConstCounterAcc = typename decltype(m_counters)::const_accessor;

    std::atomic_flag m_stopped = ATOMIC_FLAG_INIT;
};

namespace {
boost::shared_ptr<ManagerProxy> create(
    std::string ip, const unsigned short port)
{
    FLAGS_minloglevel = 1;

    auto communicator = std::make_shared<Communicator>(/*connections*/ 10,
        /*threads*/ 1, ip, port,
        /*verifyServerCertificate*/ false, /*upgrade to clproto*/ true,
        /*perform handshake*/ false);

    auto options = std::make_shared<options::Options>();
    std::vector<std::string> optionsTokens;
    std::string optionsString = std::string("oneclient -H ") + ip +
        " -t TOKEN --provider-timeout 5 " + " mountpoint";
    boost::split(optionsTokens, optionsString, boost::is_any_of(" "),
        boost::token_compress_on);

    std::vector<const char *> cmdArgs;
    std::transform(optionsTokens.begin(), optionsTokens.end(),
        std::back_inserter(cmdArgs), [](auto &s) { return s.c_str(); });

    options->parse(cmdArgs.size(), cmdArgs.data());

    auto context =
        std::make_shared<Context<one::communication::Communicator>>();
    context->setScheduler(std::make_shared<Scheduler>(1));
    context->setCommunicator(communicator);
    context->setOptions(std::move(options));
    communicator->setScheduler(context->scheduler());
    communicator->connect();

    return boost::make_shared<ManagerProxy>(context);
}
}

BOOST_PYTHON_MODULE(events)
{
    class_<ManagerProxy, boost::noncopyable>("Manager", no_init)
        .def("__init__", make_constructor(create))
        .def("stop", &ManagerProxy::stop)
        .def("emitFileRead", &ManagerProxy::emitFileRead)
        .def("emitFileWritten", &ManagerProxy::emitFileWritten)
        .def("emitFileTruncated", &ManagerProxy::emitFileTruncated)
        .def("subscribeFileRead", &ManagerProxy::subscribeFileRead)
        .def("subscribeFileWritten", &ManagerProxy::subscribeFileWritten)
        .def(
            "subscribeFileAttrChanged", &ManagerProxy::subscribeFileAttrChanged)
        .def("subscribeFileLocationChanged",
            &ManagerProxy::subscribeFileLocationChanged)
        .def(
            "subscribeFilePermChanged", &ManagerProxy::subscribeFilePermChanged)
        .def("subscribeFileRenamed", &ManagerProxy::subscribeFileRenamed)
        .def("subscribeFileRemoved", &ManagerProxy::subscribeFileRemoved)
        .def("subscribeQuotaExceeded", &ManagerProxy::subscribeQuotaExceeded)
        .def("unsubscribe", &ManagerProxy::unsubscribe)
        .def("getHandlerCallsNumber", &ManagerProxy::getHandlerCallsNumber)
        .def("existsSubscription", &ManagerProxy::existsSubscription);
}
