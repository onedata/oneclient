/**
 * @file proxyIOProxy.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "proxyIOHelper.h"

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
              std::make_unique<one::helpers::ProxyIOHelper>(
                       std::unordered_map<std::string, std::string>{
                           {"storage_id", storageId}},
                       m_communicator),
              *m_scheduler}
    {
        m_communicator.setScheduler(m_scheduler);
        m_communicator.connect();
    }

    one::helpers::CTXPtr open(std::string fileId,
        const std::unordered_map<std::string, std::string> &parameters)
    {
        ReleaseGIL guard;
        auto ctx = m_helper.createCTX(parameters);
        m_helper.sh_open(ctx, fileId, 0);
        return ctx;
    }

    int write(one::helpers::CTXPtr ctx, std::string fileId, std::string data,
        int offset)
    {
        ReleaseGIL guard;
        return m_helper.sh_write(
            std::move(ctx), fileId, asio::buffer(data), offset);
    }

    std::string read(
        one::helpers::CTXPtr ctx, std::string fileId, int offset, int size)
    {
        ReleaseGIL guard;
        std::string buffer(size, '\0');
        m_helper.sh_read(std::move(ctx), fileId, asio::buffer(buffer), offset);
        return buffer;
    }

    void release(one::helpers::CTXPtr ctx, std::string fileId)
    {
        ReleaseGIL guard;
        m_helper.sh_release(std::move(ctx), fileId);
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

one::helpers::CTXPtr raw_open(tuple args, dict kwargs)
{
    std::string fileId = extract<std::string>(args[1]);
    dict parametersDict = extract<dict>(args[2]);

    std::unordered_map<std::string, std::string> parametersMap;
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
    class_<one::helpers::IStorageHelperCTX, one::helpers::CTXPtr>(
        "CTX", no_init);

    class_<BufferAgentProxy, boost::noncopyable>("BufferAgentProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("open", raw_function(raw_open))
        .def("read", &BufferAgentProxy::read)
        .def("write", &BufferAgentProxy::write)
        .def("release", &BufferAgentProxy::release);
}
