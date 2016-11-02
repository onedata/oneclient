/**
 * @file swiftProxy.cc
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
#include <iostream>

#include <algorithm>
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

class SwiftProxy {
public:
    SwiftProxy(std::string authUrl, std::string containerName,
        std::string tenantName, std::string userName, std::string password,
        std::size_t threadNumber, std::size_t blockSize)
        : m_service{threadNumber}
        , m_idleWork{asio::make_work(m_service)}
        , m_helper{std::make_shared<one::helpers::KeyValueAdapter>(
              std::make_unique<one::helpers::SwiftHelper>(
                  std::unordered_map<std::string, std::string>(
                      {{"auth_url", std::move(authUrl)},
                          {"container_name", std::move(containerName)},
                          {"tenant_name", std::move(tenantName)}})),
              m_service, m_locks, blockSize)}
        , m_ctx{m_helper->createCTX({})}
    {
        auto ctx =
            std::dynamic_pointer_cast<one::helpers::SwiftHelperCTX>(m_ctx);
        ctx->setUserCTX({{"user_name", std::move(userName)},
            {"password", std::move(password)}});

        std::generate_n(std::back_inserter(m_workers), threadNumber, [=] {
            std::thread t{[=] {
                one::etls::utils::nameThread("SwiftProxy");
                m_service.run();
            }};

            return t;
        });
    }

    ~SwiftProxy()
    {
        m_service.stop();
        for (auto &t : m_workers)
            t.join();
    }

    void unlink(std::string fileId) { m_helper->sh_unlink(m_ctx, fileId); }

    std::string read(std::string fileId, int offset, int size)
    {
        std::string buffer(size, '\0');
        auto read =
            m_helper->sh_read(m_ctx, fileId, asio::buffer(buffer), offset);
        return std::string{
            asio::buffer_cast<char *>(read), asio::buffer_size(read)};
    }

    std::size_t write(std::string fileId, std::string data, int offset)
    {
        return m_helper->sh_write(m_ctx, fileId, asio::buffer(data), offset);
    }

    void truncate(std::string fileId, int offset)
    {
        m_helper->sh_truncate(m_ctx, fileId, offset);
    }

private:
    asio::io_service m_service;
    asio::executor_work<asio::io_service::executor_type> m_idleWork;
    std::vector<std::thread> m_workers;
    one::helpers::KeyValueAdapter::Locks m_locks;
    std::shared_ptr<one::helpers::IStorageHelper> m_helper;
    std::shared_ptr<one::helpers::IStorageHelperCTX> m_ctx;
};

namespace {
boost::shared_ptr<SwiftProxy> create(std::string authUrl,
    std::string containerName, std::string tenantName, std::string userName,
    std::string password, std::size_t threadNumber, std::size_t blockSize)
{
    return boost::make_shared<SwiftProxy>(std::move(authUrl),
        std::move(containerName), std::move(tenantName), std::move(userName),
        std::move(password), threadNumber, blockSize);
}
}

BOOST_PYTHON_MODULE(swift)
{
    class_<SwiftProxy, boost::noncopyable>("SwiftProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("unlink", &SwiftProxy::unlink)
        .def("read", &SwiftProxy::read)
        .def("write", &SwiftProxy::write)
        .def("truncate", &SwiftProxy::truncate);
}
