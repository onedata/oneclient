/**
 * @file s3Proxy.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "keyValueAdapter.h"
#include "s3Helper.h"
#include "utils.hpp"

#include <asio/buffer.hpp>
#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>

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

class S3Proxy {
public:
    S3Proxy(std::string scheme, std::string hostName, std::string bucketName,
        std::string accessKey, std::string secretKey, std::size_t threadNumber,
        std::size_t blockSize)
        : m_service{threadNumber}
        , m_idleWork{asio::make_work(m_service)}
        , m_helper{std::make_shared<one::helpers::KeyValueAdapter>(
              std::make_unique<one::helpers::S3Helper>(
                  std::unordered_map<std::string, std::string>(
                      {{"scheme", std::move(scheme)},
                          {"host_name", std::move(hostName)},
                          {"bucket_name", std::move(bucketName)}})),
              m_service, m_locks, blockSize)}
        , m_ctx{m_helper->createCTX({})}
    {
        auto ctx = std::dynamic_pointer_cast<one::helpers::S3HelperCTX>(m_ctx);
        ctx->setUserCTX({{"access_key", std::move(accessKey)},
            {"secret_key", std::move(secretKey)}});

        std::generate_n(std::back_inserter(m_workers), threadNumber, [=] {
            std::thread t{[=] {
                one::etls::utils::nameThread("S3Proxy");
                m_service.run();
            }};

            return t;
        });
    }

    ~S3Proxy()
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
boost::shared_ptr<S3Proxy> create(std::string scheme, std::string hostName,
    std::string bucketName, std::string accessKey, std::string secretKey,
    std::size_t threadNumber, std::size_t blockSize)
{
    return boost::make_shared<S3Proxy>(std::move(scheme), std::move(hostName),
        std::move(bucketName), std::move(accessKey), std::move(secretKey),
        threadNumber, blockSize);
}
}

BOOST_PYTHON_MODULE(s3)
{
    class_<S3Proxy, boost::noncopyable>("S3Proxy", no_init)
        .def("__init__", make_constructor(create))
        .def("unlink", &S3Proxy::unlink)
        .def("read", &S3Proxy::read)
        .def("write", &S3Proxy::write)
        .def("truncate", &S3Proxy::truncate);
}
