/**
 * @file proxyIOProxy.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "proxyHelper.h"

#include "buffering/bufferAgent.h"
#include "communication/communicator.h"

#include <asio/buffer.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/extract.hpp>
#include <boost/python/raw_function.hpp>

#include <string>

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

class BufferAgentProxy {
public:
    BufferAgentProxy(
        std::string storageId, std::string host, const unsigned short port)
        : m_communicator{1, host, port, false,
              one::communication::createConnection}
        , m_scheduler{std::make_shared<one::Scheduler>(1)}
        , m_helper{one::helpers::buffering::BufferLimits{},
              std::make_shared<one::helpers::ProxyHelper>(
                  storageId, m_communicator),
              *m_scheduler}
    {
        m_communicator.setScheduler(m_scheduler);
        m_communicator.connect();
    }

    one::helpers::FileHandlePtr open(std::string fileId,
        const std::unordered_map<folly::fbstring, folly::fbstring> &parameters)
    {
        ReleaseGIL guard;
        return m_helper.open(fileId, 0, parameters).get();
    }

    int write(one::helpers::FileHandlePtr handle, std::string data, int offset)
    {
        ReleaseGIL guard;
        folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
        buf.append(data);
        return handle->write(offset, std::move(buf)).get();
    }

    std::string read(one::helpers::FileHandlePtr handle, int offset, int size)
    {
        ReleaseGIL guard;
        return handle->read(offset, size)
            .then([&](const folly::IOBufQueue &buf) {
                std::string data;
                buf.appendToString(data);
                return data;
            })
            .get();
    }

    void release(one::helpers::FileHandlePtr handle)
    {
        ReleaseGIL guard;
        handle->release().get();
    }

private:
    one::communication::Communicator m_communicator;
    std::shared_ptr<one::Scheduler> m_scheduler;
    one::helpers::buffering::BufferAgent m_helper;
};

namespace {
boost::shared_ptr<BufferAgentProxy> create(
    std::string storageId, std::string host, int port)
{
    return boost::make_shared<BufferAgentProxy>(storageId, host, port);
}
}

one::helpers::FileHandlePtr raw_open(tuple args, dict kwargs)
{
    std::string fileId = extract<std::string>(args[1]);
    dict parametersDict = extract<dict>(args[2]);

    std::unordered_map<folly::fbstring, folly::fbstring> parametersMap;
    list keys = parametersDict.keys();
    for (int i = 0; i < len(keys); ++i) {
        std::string key = extract<std::string>(keys[i]);
        std::string val = extract<std::string>(parametersDict[key]);
        parametersMap[key] = val;
    }

    return extract<BufferAgentProxy &>(args[0])().open(fileId, parametersMap);
}

BOOST_PYTHON_MODULE(buffer_agent)
{
    class_<one::helpers::FileHandlePtr>("FileHandle", no_init);

    class_<BufferAgentProxy, boost::noncopyable>("BufferAgentProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("open", raw_function(raw_open))
        .def("read", &BufferAgentProxy::read)
        .def("write", &BufferAgentProxy::write)
        .def("release", &BufferAgentProxy::release);
}
