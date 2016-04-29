/**
 * @file s3Proxy.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Helper.h"

#include <asio/buffer.hpp>
#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>

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

class S3Proxy {
public:
    S3Proxy(std::string hostName, std::string bucketName, std::string accessKey,
        std::string secretKey)
        : m_service{1}
        , m_idleWork{asio::make_work(m_service)}
        , m_worker{[=] { m_service.run(); }}
        , m_helper{{{"host_name", std::move(hostName)},
                       {"bucket_name", std::move(bucketName)},
                       {"access_key", std::move(accessKey)},
                       {"secret_key", std::move(secretKey)}},
              m_service}
    {
        auto rawCTX = m_helper.createCTX({});
        m_ctx = std::dynamic_pointer_cast<one::helpers::S3HelperCTX>(rawCTX);
        if (m_ctx == nullptr)
            throw std::system_error{
                std::make_error_code(std::errc::invalid_argument)};
    }

    ~S3Proxy()
    {
        m_service.stop();
        m_worker.join();
    }

    void unlink(std::string fileId) { m_helper.sh_unlink(*m_ctx, fileId); }

    std::string read(std::string fileId, int offset, int size)
    {
        std::string buffer(size, '\0');
        m_helper.sh_read(*m_ctx, fileId, asio::buffer(buffer), offset);
        return buffer;
    }

    int write(std::string fileId, std::string data, int offset)
    {
        return m_helper.sh_write(*m_ctx, fileId, asio::buffer(data), offset);
    }

    void truncate(std::string fileId, int offset)
    {
        m_helper.sh_truncate(*m_ctx, fileId, offset);
    }

private:
    asio::io_service m_service;
    asio::executor_work<asio::io_service::executor_type> m_idleWork;
    std::thread m_worker;
    one::helpers::S3Helper m_helper;
    std::shared_ptr<one::helpers::S3HelperCTX> m_ctx;
};

namespace {
boost::shared_ptr<S3Proxy> create(std::string hostName, std::string bucketName,
    std::string accessKey, std::string secretKey)
{
    return boost::make_shared<S3Proxy>(std::move(hostName),
        std::move(bucketName), std::move(accessKey), std::move(secretKey));
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
