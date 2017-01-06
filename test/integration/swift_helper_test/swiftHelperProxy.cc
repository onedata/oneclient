/**
 * @file swiftHelperProxy.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "keyValueAdapter.h"
#include "swiftHelper.h"
#include "utils.hpp"

#include <asio/buffer.hpp>
#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <folly/ThreadName.h>

#include <algorithm>
#include <iostream>
#include <thread>
#include <unordered_map>
#include <vector>

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

class SwiftHelperProxy {
public:
    SwiftHelperProxy(std::string authUrl, std::string containerName,
        std::string tenantName, std::string userName, std::string password,
        std::size_t threadNumber, std::size_t blockSize)
        : m_service{threadNumber}
        , m_idleWork{asio::make_work(m_service)}
        , m_helper{std::make_shared<one::helpers::KeyValueAdapter>(
              std::make_shared<one::helpers::SwiftHelper>(
                  std::move(containerName), authUrl, tenantName, userName,
                  password),
              std::make_shared<one::AsioExecutor>(m_service), blockSize)}
    {
        std::generate_n(std::back_inserter(m_workers), threadNumber, [=] {
            std::thread t{[=] {
                folly::setThreadName("SwiftHelperProxy");
                m_service.run();
            }};

            return t;
        });
    }

    ~SwiftHelperProxy()
    {
        m_service.stop();
        for (auto &t : m_workers)
            t.join();
    }

    void unlink(std::string fileId) { m_helper->unlink(fileId).get(); }

    std::string read(std::string fileId, int offset, int size)
    {
        return m_helper->open(fileId, 0, {})
            .then([&](one::helpers::FileHandlePtr handle) {
                return handle->read(offset, size);
            })
            .then([&](const folly::IOBufQueue &buf) {
                std::string data;
                buf.appendToString(data);
                return data;
            })
            .get();
    }

    std::size_t write(std::string fileId, std::string data, int offset)
    {
        return m_helper->open(fileId, 0, {})
            .then([&](one::helpers::FileHandlePtr handle) {
                folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
                buf.append(data);
                return handle->write(offset, std::move(buf));
            })
            .get();
    }

    void truncate(std::string fileId, int offset)
    {
        m_helper->truncate(fileId, offset).get();
    }

private:
    asio::io_service m_service;
    asio::executor_work<asio::io_service::executor_type> m_idleWork;
    std::vector<std::thread> m_workers;
    std::shared_ptr<one::helpers::StorageHelper> m_helper;
};

namespace {
boost::shared_ptr<SwiftHelperProxy> create(std::string authUrl,
    std::string containerName, std::string tenantName, std::string userName,
    std::string password, std::size_t threadNumber, std::size_t blockSize)
{
    return boost::make_shared<SwiftHelperProxy>(std::move(authUrl),
        std::move(containerName), std::move(tenantName), std::move(userName),
        std::move(password), threadNumber, blockSize);
}
}

BOOST_PYTHON_MODULE(swift_helper)
{
    class_<SwiftHelperProxy, boost::noncopyable>("SwiftHelperProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("unlink", &SwiftHelperProxy::unlink)
        .def("read", &SwiftHelperProxy::read)
        .def("write", &SwiftHelperProxy::write)
        .def("truncate", &SwiftHelperProxy::truncate);
}
