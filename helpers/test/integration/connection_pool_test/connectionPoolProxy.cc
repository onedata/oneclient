/**
 * @file connectionPoolProxy.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/connectionPool.h"
#include "communication/persistentConnection.h"

#include <boost/make_shared.hpp>
#include <boost/python.hpp>
#include <tbb/concurrent_queue.h>

#include <atomic>
#include <string>

using namespace boost::python;
using namespace one::communication;

std::unique_ptr<Connection> createRawConnection(std::string host,
    const unsigned short port, asio::io_service &ioService,
    std::shared_ptr<asio::ssl::context> context,
    std::function<void(std::string)> onMessage,
    std::function<void(Connection &)> onReady,
    std::function<std::string()> getHandshake,
    std::function<std::error_code(std::string)> onHandshakeResponse,
    std::function<void(std::error_code)> onHandshakeDone)
{
    return std::make_unique<PersistentConnection>(std::move(host), port,
        ioService, std::move(context), std::move(onMessage), std::move(onReady),
        std::move(getHandshake), std::move(onHandshakeResponse),
        std::move(onHandshakeDone), false);
}

class ConnectionPoolProxy {
public:
    ConnectionPoolProxy(const std::size_t conn, const std::size_t workers,
        std::string host, const unsigned short port)
        : m_pool{
              conn, workers, std::move(host), port, false, createRawConnection}
    {
        m_pool.setOnMessageCallback([this](std::string msg) {
            m_messages.emplace(std::move(msg));
            ++m_size;
        });

        m_pool.connect();
    }

    void send(const std::string &msg)
    {
        m_pool.send(msg, [](auto) {}, int{});
    }

    std::string popMessage()
    {
        std::string msg;
        m_messages.try_pop(msg);
        return msg;
    }

    size_t size() { return m_size; }

private:
    ConnectionPool m_pool;
    std::atomic<std::size_t> m_size{0};
    tbb::concurrent_queue<std::string> m_messages;
};

namespace {
boost::shared_ptr<ConnectionPoolProxy> create(
    int conn, int workers, std::string host, int port)
{
    return boost::make_shared<ConnectionPoolProxy>(
        conn, workers, std::move(host), port);
}
}

BOOST_PYTHON_MODULE(connection_pool)
{
    class_<ConnectionPoolProxy, boost::noncopyable>(
        "ConnectionPoolProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("send", &ConnectionPoolProxy::send)
        .def("popMessage", &ConnectionPoolProxy::popMessage)
        .def("size", &ConnectionPoolProxy::size);
}
