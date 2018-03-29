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
#include "scheduler.h"

#include "messages.pb.h"

#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <tbb/concurrent_hash_map.h>

#include <chrono>
#include <memory>
#include <unordered_map>

using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace one::communication;
using namespace boost::python;

class ManagerProxy {
public:
    ManagerProxy(std::shared_ptr<Context> context)
        : m_context{std::move(context)}
        , m_manager{m_context}
    {
    }

    ~ManagerProxy() { m_context->communicator()->stop(); }

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
        return subscribeIOEvents<clproto::FileReadSubscription,
            FileReadSubscription>(counterThr, timeThr);
    }

    std::int64_t subscribeFileWritten(
        std::int64_t counterThr, std::int64_t timeThr)
    {
        return subscribeIOEvents<clproto::FileWrittenSubscription,
            FileWrittenSubscription>(counterThr, timeThr);
    }

    std::int64_t subscribeFileAttrChanged(
        std::string fileUuid, std::int64_t timeThr)
    {
        FileAttrChangedSubscription sub{std::move(fileUuid),
            std::chrono::milliseconds{timeThr},
            [this](auto) { this->incCounter("file_attr_changed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeFileLocationChanged(
        std::string fileUuid, std::int64_t timeThr)
    {
        FileLocationChangedSubscription sub{std::move(fileUuid),
            std::chrono::milliseconds{timeThr},
            [this](auto) { this->incCounter("file_location_changed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeFilePermChanged(std::string fileUuid)
    {
        FilePermChangedSubscription sub{std::move(fileUuid),
            [this](auto) { this->incCounter("file_perm_changed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeFileRenamed(std::string fileUuid)
    {
        FileRenamedSubscription sub{std::move(fileUuid),
            [this](auto) { this->incCounter("file_renamed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeFileRemoved(std::string fileUuid)
    {
        FileRemovedSubscription sub{std::move(fileUuid),
            [this](auto) { this->incCounter("file_removed"); }};
        return m_manager.subscribe(sub);
    }

    std::int64_t subscribeQuotaExceeded()
    {
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
    std::shared_ptr<Context> m_context;
    Manager m_manager;

    using CounterAcc = typename decltype(m_counters)::accessor;
    using ConstCounterAcc = typename decltype(m_counters)::const_accessor;
};

namespace {
boost::shared_ptr<ManagerProxy> create(
    std::string ip, const unsigned short port)
{
    FLAGS_minloglevel = 1;

    auto communicator = std::make_shared<Communicator>(/*connections*/ 1,
        /*threads*/ 1, std::move(ip), port,
        /*verifyServerCertificate*/ false, createConnection);

    auto context = std::make_shared<Context>();
    context->setScheduler(std::make_shared<Scheduler>(1));
    context->setCommunicator(communicator);
    communicator->setScheduler(context->scheduler());
    communicator->connect();

    return boost::make_shared<ManagerProxy>(context);
}
}

BOOST_PYTHON_MODULE(events)
{
    class_<ManagerProxy, boost::noncopyable>("Manager", no_init)
        .def("__init__", make_constructor(create))
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
