/**
 * @file connectionProxy.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/connection.h"
#include "ioServiceExecutor.h"

#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/make_shared.hpp>
#include <boost/python.hpp>

#include <atomic>
#include <chrono>
#include <functional>
#include <memory>
#include <vector>
#include <string>
#include <thread>

using namespace one;
using namespace one::communication;
using namespace boost::python;
using namespace std::literals;
namespace p = std::placeholders;

boost::asio::ssl::context prepareContext()
{
    boost::asio::ssl::context context{boost::asio::ssl::context::tlsv12_client};
    context.set_verify_mode(boost::asio::ssl::verify_none);
    return context;
}

class ConnectionProxy {
public:
    ConnectionProxy(std::string handshake, bool acceptHandshake)
        : m_idleWork{m_ioService}
        , m_context{prepareContext()}
        , m_worker{[this] { m_ioService.run(); }}
        , m_getHandshake{[=] { return handshake; }}
        , m_onHandshakeResponse{[=](auto response) {
            m_handshakeResponse.set_value(response);
            return acceptHandshake;
        }}
        , m_connection{std::make_shared<Connection>(m_ioService, nullptr,
              m_context, false, m_getHandshake, m_onHandshakeResponse,
              std::bind(&ConnectionProxy::onMessageReceived, this, p::_1),
              std::bind(&ConnectionProxy::onReady, this, p::_1),
              std::bind(&ConnectionProxy::onClosed, this, p::_1, p::_2))}
    {
    }

    ~ConnectionProxy()
    {
        m_ioService.stop();
        m_worker.join();
    }

    void onMessageReceived(std::string msg)
    {
        m_message = std::move(msg);
        m_hasMessage = true;
    }

    void onReady(std::shared_ptr<Connection>) { m_isReady = true; }

    void onClosed(std::shared_ptr<Connection>, boost::exception_ptr)
    {
        m_isClosed = true;
    }

    void connect(std::string host, int port)
    {
        m_connection->connect(std::move(host), std::to_string(port));
    }

    void send(std::string message)
    {
        boost::promise<void> promise;
        m_connection->send(std::move(message), std::move(promise));
    }

    std::string getHandshakeResponse()
    {
        return m_handshakeResponse.get_future().get();
    }

    bool isClosed() const { return m_isClosed; }

    bool waitForClosed() { return waitFor(m_isClosed); }

    bool isReady() const { return m_isReady; }

    bool waitForReady()
    {
        auto ret = waitFor(m_isReady);
        m_isReady = false;
        return ret;
    }

    bool waitForMessage()
    {
        auto ret = waitFor(m_hasMessage);
        m_hasMessage = false;
        return ret;
    }

    std::string getMessage() const { return m_message; }

private:
    bool waitFor(const std::atomic<bool> &something)
    {
        auto start = std::chrono::steady_clock::now();
        while (!something) {
            if (std::chrono::steady_clock::now() > start + 5s)
                break;

            std::this_thread::sleep_for(10ms);
        }

        return something;
    }

    boost::asio::io_service m_ioService;
    boost::asio::io_service::work m_idleWork;
    boost::asio::ssl::context m_context;
    std::thread m_worker;
    boost::promise<std::string> m_handshakeResponse;
    std::function<std::string()> m_getHandshake;
    std::function<bool(std::string)> m_onHandshakeResponse;
    std::shared_ptr<Connection> m_connection;
    std::atomic<bool> m_isClosed{false};
    std::atomic<bool> m_isReady{false};
    std::atomic<bool> m_hasMessage{false};
    std::string m_message;
};

namespace {
boost::shared_ptr<ConnectionProxy> create(
    std::string handshake, bool acceptHandshake)
{
    return boost::make_shared<ConnectionProxy>(
        std::move(handshake), acceptHandshake);
}
}

void connectionProxyModule()
{
    class_<ConnectionProxy, boost::noncopyable>("ConnectionProxy", no_init)
        .def("__init__", make_constructor(create))
        .def("connect", &ConnectionProxy::connect)
        .def("send", &ConnectionProxy::send)
        .def("getHandshakeResponse", &ConnectionProxy::getHandshakeResponse)
        .def("isClosed", &ConnectionProxy::isClosed)
        .def("waitForClosed", &ConnectionProxy::waitForClosed)
        .def("isReady", &ConnectionProxy::isReady)
        .def("waitForReady", &ConnectionProxy::waitForReady)
        .def("waitForMessage", &ConnectionProxy::waitForMessage)
        .def("getMessage", &ConnectionProxy::getMessage);
}
