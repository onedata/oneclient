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
        , m_helper{
              {{"user_name", std::move(username)}, {"cluster_name", "ceph"},
                  {"mon_host", std::move(monHost)}, {"key", std::move(key)},
                  {"pool_name", std::move(poolName)}},
              m_service}
    {
        auto rawCTX = m_helper.createCTX({});
        m_ctx = std::dynamic_pointer_cast<one::helpers::CephHelperCTX>(rawCTX);
        if (m_ctx == nullptr)
            throw std::system_error{
                std::make_error_code(std::errc::invalid_argument)};
    }

    ~CephProxy()
    {
        m_service.stop();
        m_worker.join();
    }

    void unlink(std::string fileId)
    {
        ReleaseGIL guard;

        auto p = makePromise<void>();
        m_helper.ash_unlink(m_ctx, fileId, std::bind(&CephProxy::setVoidPromise,
                                               this, p, std::placeholders::_1));

        return getFuture(std::move(p));
    }

    std::string read(std::string fileId, int offset, int size)
    {
        ReleaseGIL guard;
        std::string buffer(size, '\0');
        m_helper.sh_read(m_ctx, fileId, asio::buffer(buffer), offset);
        return buffer;
    }

    int write(std::string fileId, std::string data, int offset)
    {
        ReleaseGIL guard;
        return m_helper.sh_write(m_ctx, fileId, asio::buffer(data), offset);
    }

    void truncate(std::string fileId, int offset)
    {
        ReleaseGIL guard;

        auto p = makePromise<void>();
        m_helper.ash_truncate(
            m_ctx, fileId, offset, std::bind(&CephProxy::setVoidPromise, this,
                                       p, std::placeholders::_1));

        return getFuture(std::move(p));
    }

private:
    void setVoidPromise(
        std::shared_ptr<std::promise<void>> p, one::helpers::error_t e)
    {
        if (e) {
            p->set_exception(std::make_exception_ptr(std::system_error(e)));
        }
        else {
            p->set_value();
        }
    }

    template <class T> std::shared_ptr<std::promise<T>> makePromise()
    {
        return std::make_shared<std::promise<T>>();
    }

    template <class T> T getFuture(std::shared_ptr<std::promise<T>> p)
    {
        using namespace std::literals;
        auto f = p->get_future();
        if (f.wait_for(2s) != std::future_status::ready)
            throw std::system_error{std::make_error_code(std::errc::timed_out)};
        return f.get();
    }

    asio::io_service m_service;
    asio::executor_work<asio::io_service::executor_type> m_idleWork;
    std::thread m_worker;
    one::helpers::CephHelper m_helper;
    std::shared_ptr<one::helpers::CephHelperCTX> m_ctx;
};

namespace {
boost::shared_ptr<CephProxy> create(std::string monHost, std::string username,
    std::string key, std::string poolName)
{
    return boost::make_shared<CephProxy>(std::move(monHost),
        std::move(username), std::move(key), std::move(poolName));
}
}

BOOST_PYTHON_MODULE(ceph)
{
    class_<CephProxy, boost::noncopyable>("CephProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("unlink", &CephProxy::unlink)
        .def("read", &CephProxy::read)
        .def("write", &CephProxy::write)
        .def("truncate", &CephProxy::truncate);
}
