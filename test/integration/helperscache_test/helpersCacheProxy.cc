/**
 * @file helpersCacheProxy.cc
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/helpersCache.h"
#include "posixHelper.h"

#include "communication/communicator.h"
#include "context.h"
#include "fslogic/fsLogic.h"
#include "fslogic/withUuids.h"
#include "messages/configuration.h"
#include "options/options.h"
#include "scheduler.h"

#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/filesystem.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>
#include <fuse.h>

#include <memory>

using namespace one;
using namespace one::client;
using namespace one::client::cache;
using namespace one::communication;
using namespace one::helpers;
using namespace boost::python;
using namespace std::literals;

class ReleaseGIL {
public:
    ReleaseGIL()
        : threadState{PyEval_SaveThread(), PyEval_RestoreThread}
    {
    }

private:
    std::unique_ptr<PyThreadState, decltype(&PyEval_RestoreThread)> threadState;
};

class FileHandleProxy {
public:
    FileHandleProxy(std::shared_ptr<FileHandle> fileHandle)
        : m_fileHandle(std::move(fileHandle))
    {
    }

    std::string read(const off_t offset, const std::size_t size)
    {
        ReleaseGIL guard;
        return m_fileHandle->read(offset, size)
            .then([](folly::IOBufQueue &&buf) {
                std::string data;
                buf.appendToString(data);
                return data;
            })
            .get();
    }

    size_t write(const off_t offset, std::string data)
    {
        ReleaseGIL guard;
        folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
        buf.append(data);

        return m_fileHandle->write(offset, std::move(buf)).get();
    }

private:
    std::shared_ptr<FileHandle> m_fileHandle;
};

class HelpersCacheProxy {
public:
    HelpersCacheProxy(std::shared_ptr<Context> context)
        : m_helpersCache{std::make_shared<HelpersCache>(
              *context->communicator(), *context->scheduler(),
              *context->options())}
        , m_context{context}
    {
    }

    ~HelpersCacheProxy()
    {
        ReleaseGIL guard;
        m_context->communicator()->stop();
    }

    void mknod(std::string fileUuid, std::string spaceId, std::string storageId,
        bool forceProxyIO)
    {
        m_helpersCache->get(fileUuid, spaceId, storageId, forceProxyIO)
            .then([fileUuid](auto &helperPtr) {
                helperPtr->mknod(fileUuid, S_IFREG | 0666, {}, 0);
            })
            .get();
    }

    boost::shared_ptr<FileHandleProxy> open(std::string fileUuid,
        std::string spaceId, std::string storageId, bool forceProxyIO)
    {
        auto handle =
            m_helpersCache->get(fileUuid, spaceId, storageId, forceProxyIO)
                .then([fileUuid](auto &helperPtr) {
                    return helperPtr->open(fileUuid, O_RDWR | O_CREAT, {});
                })
                .get();

        return boost::make_shared<FileHandleProxy>(std::move(handle));
    }

    bool isDirectIOForced() { return m_context->options()->isDirectIOForced(); }

    bool isProxyIOForced() { return m_context->options()->isProxyIOForced(); }

    bool get(std::string fileUuid, std::string spaceId, std::string storageId,
        bool forceProxyIO)
    {
        ReleaseGIL guard;

        auto helper =
            m_helpersCache->get(fileUuid, spaceId, storageId, forceProxyIO)
                .get();

        return !!helper;
    }

    std::string getAccessType(std::string storageId)
    {
        ReleaseGIL guard;

        auto accessType = m_helpersCache->getAccessType(storageId);

        if (accessType == HelpersCache::AccessType::DIRECT)
            return "direct";
        else if (accessType == HelpersCache::AccessType::PROXY)
            return "proxy";
        else
            return "unknown";
    }

    void refreshHelperParameters(std::string storageId, std::string spaceId)
    {
        ReleaseGIL guard;

        m_helpersCache->refreshHelperParameters(storageId, spaceId);
    }

private:
    std::shared_ptr<HelpersCache> m_helpersCache;
    std::shared_ptr<Context> m_context;
};

namespace {
boost::shared_ptr<HelpersCacheProxy> create(
    std::string ip, int port, std::string options)
{
    FLAGS_minloglevel = 1;

    std::vector<std::string> argvStrings;
    std::vector<const char *> argv;
    boost::algorithm::split(argvStrings, options, boost::is_any_of(" \t"));

    argv.emplace_back("oneclient");
    for (auto opt : argvStrings)
        argv.emplace_back(strdup(opt.c_str()));

    auto communicator = std::make_shared<Communicator>(/*connections*/ 10,
        /*threads*/ 4, ip, port, /*verifyServerCertificate*/ false,
        /*upgrade to clproto*/ true, /*perform handshake*/ false);

    auto context = std::make_shared<Context>();
    context->setScheduler(std::make_shared<Scheduler>(4));
    context->setCommunicator(communicator);
    const auto globalConfigPath = boost::filesystem::unique_path();
    context->setOptions(std::make_shared<options::Options>());
    context->options()->parse(argv.size(), &argv[0]);

    communicator->setScheduler(context->scheduler());
    communicator->connect();

    return boost::make_shared<HelpersCacheProxy>(context);
}

void translate(const std::errc &err)
{
    PyErr_SetString(
        PyExc_RuntimeError, std::make_error_code(err).message().c_str());
}
}

BOOST_PYTHON_MODULE(helperscache)
{
    PyEval_InitThreads();
    register_exception_translator<std::errc>(&translate);

    class_<std::vector<std::string>>("vector").def(
        vector_indexing_suite<std::vector<std::string>>());

    class_<FileHandleProxy, boost::noncopyable>("FileHandleProxy", no_init)
        .def("read", &FileHandleProxy::read)
        .def("write", &FileHandleProxy::write);

    boost::python::register_ptr_to_python<boost::shared_ptr<FileHandleProxy>>();

    class_<HelpersCacheProxy, boost::noncopyable>("HelpersCacheProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("get", &HelpersCacheProxy::get)
        .def("get_access_type", &HelpersCacheProxy::getAccessType)
        .def("refresh_helper_parameters",
            &HelpersCacheProxy::refreshHelperParameters)
        .def("open", &HelpersCacheProxy::open)
        .def("mknod", &HelpersCacheProxy::mknod)
        .def("is_directio_forced", &HelpersCacheProxy::isDirectIOForced)
        .def("is_proxyio_forced", &HelpersCacheProxy::isProxyIOForced);
}
