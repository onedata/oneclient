/**
 * @file cephProxy.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cephHelper.h"

#include <asio/buffer.hpp>
#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <boost/python/extract.hpp>
#include <boost/python/raw_function.hpp>

#include <chrono>
#include <future>
#include <string>
#include <thread>

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

class CephProxy {
public:
    CephProxy(std::string monHost, std::string username, std::string key,
        std::string poolName)
        : m_service{1}
        , m_idleWork{asio::make_work(m_service)}
        , m_worker{[=] { m_service.run(); }}
        , m_helper{std::make_shared<one::helpers::CephHelper>("ceph", monHost,
              poolName, username, key,
              std::make_unique<one::AsioExecutor>(m_service))}
    {
    }

    ~CephProxy()
    {
        m_service.stop();
        m_worker.join();
    }

    void unlink(std::string fileId)
    {
        ReleaseGIL guard;
        m_helper->unlink(fileId).get();
    }

    std::string read(std::string fileId, int offset, int size)
    {
        ReleaseGIL guard;
        return m_helper->open(fileId, 0, {})
            .then([&](one::helpers::FileHandlePtr handle) {
                auto buf = handle->read(offset, size).get();
                std::string data;
                buf.appendToString(data);
                return data;
            })
            .get();
    }

    int write(std::string fileId, std::string data, int offset)
    {
        ReleaseGIL guard;
        return m_helper->open(fileId, 0, {})
            .then([&](one::helpers::FileHandlePtr handle) {
                folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
                buf.append(data);
                return handle->write(offset, std::move(buf)).get();
            })
            .get();
    }

    void truncate(std::string fileId, int offset)
    {
        ReleaseGIL guard;
        m_helper->truncate(fileId, offset).get();
    }

private:
    asio::io_service m_service;
    asio::executor_work<asio::io_service::executor_type> m_idleWork;
    std::thread m_worker;
    std::shared_ptr<one::helpers::CephHelper> m_helper;
};

namespace {
boost::shared_ptr<CephProxy> create(std::string monHost, std::string username,
    std::string key, std::string poolName)
{
    return boost::make_shared<CephProxy>(std::move(monHost),
        std::move(username), std::move(key), std::move(poolName));
}
} // namespace

BOOST_PYTHON_MODULE(ceph)
{
    class_<CephProxy, boost::noncopyable>("CephProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("unlink", &CephProxy::unlink)
        .def("read", &CephProxy::read)
        .def("write", &CephProxy::write)
        .def("truncate", &CephProxy::truncate);
}
