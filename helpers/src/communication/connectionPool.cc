/**
 * @file connectionPool.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "connectionPool.h"

#include "exception.h"
#include "connection.h"
#include "logging.h"
#include "cert/certificateData.h"

#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <openssl/ssl.h>

#include <algorithm>
#include <array>
#include <iterator>
#include <tuple>

using namespace std::placeholders;
using namespace std::literals::chrono_literals;
using steady_timer =
    boost::asio::basic_waitable_timer<std::chrono::steady_clock>;

static constexpr size_t OUTBOX_SIZE = 1000;
static constexpr auto RECREATE_DELAY = 1s;

namespace one {
namespace communication {

ConnectionPool::ConnectionPool(const unsigned int connectionsNumber,
    std::string host, std::string service, const bool verifyServerCertificate)
    : m_connectionsNumber{connectionsNumber}
    , m_host{std::move(host)}
    , m_service{std::move(service)}
    , m_verifyServerCertificate{verifyServerCertificate}
    , m_idleWork{m_ioService}
    , m_blockingStrand{m_ioService}
    , m_connectionsStrand{m_ioService}
    , m_context{boost::asio::ssl::context::tlsv12_client}
{
    m_outbox.set_capacity(OUTBOX_SIZE);
}

void ConnectionPool::connect()
{
    m_workers.clear();
    std::generate_n(std::back_inserter(m_workers),
        std::max<int>(std::thread::hardware_concurrency(), 2), [=] {
            return std::thread{[=] {
                try {
                    m_ioService.run();
                }
                catch (tbb::user_abort &) {
                    return;
                }
            }};
        });

    try {
        m_context.set_options(boost::asio::ssl::context::default_workarounds |
            boost::asio::ssl::context::no_sslv2 |
            boost::asio::ssl::context::no_sslv3 |
            boost::asio::ssl::context::single_dh_use);

        m_context.set_default_verify_paths();
        m_context.set_verify_mode(m_verifyServerCertificate
                ? boost::asio::ssl::verify_peer
                : boost::asio::ssl::verify_none);

        SSL_CTX *ssl_ctx = m_context.native_handle();
        auto mode = SSL_CTX_get_session_cache_mode(ssl_ctx);
        mode |= SSL_SESS_CACHE_CLIENT;
        SSL_CTX_set_session_cache_mode(ssl_ctx, mode);

        if (m_certificateData)
            m_certificateData->initContext(m_context);
    }
    catch (const boost::system::error_code &ec) {
        throw ConnectionError{ec.message()};
    }

    for (auto i = 0u; i < m_connectionsNumber; ++i)
        createConnection();
}

void ConnectionPool::setHandshake(std::function<std::string()> getHandshake,
    std::function<bool(std::string)> onHandshakeResponse)
{
    m_getHandshake = std::move(getHandshake);
    m_onHandshakeResponse = std::move(onHandshakeResponse);
}

void ConnectionPool::setOnMessageCallback(
    std::function<void(std::string)> onMessage)
{
    m_onMessage = std::move(onMessage);
}

void ConnectionPool::setCertificateData(
    std::shared_ptr<cert::CertificateData> certificateData)
{
    m_certificateData = std::move(certificateData);
}

std::future<void> ConnectionPool::send(std::string message, const int)
{
    std::promise<void> promise;
    auto future = promise.get_future();
    auto data = std::make_shared<std::tuple<std::string, std::promise<void>>>(
        std::forward_as_tuple(std::move(message), std::move(promise)));

    m_outbox.emplace(std::move(data));
    return future;
}

void ConnectionPool::onMessageReceived(std::string message)
{
    m_ioService.post([ this, message = std::move(message) ] {
        m_onMessage(message);
    });
}

void ConnectionPool::onConnectionReady(std::shared_ptr<Connection> conn)
{
    m_blockingStrand.post([ this, c = std::weak_ptr<Connection>{conn} ] {
        std::shared_ptr<SendTask> task;
        if (!m_rejects.try_pop(task))
            m_outbox.pop(task);

        if (auto conn = c.lock()) {
            conn->send(
                std::move(std::get<0>(*task)), std::move(std::get<1>(*task)));
        }
        else {
            m_rejects.emplace(std::move(task));
        }
    });
}

/// TODO: Take an exception pointer (and then what?)
void ConnectionPool::onConnectionClosed(std::shared_ptr<Connection> conn)
{
    m_connectionsStrand.post([this, conn] { m_connections.erase(conn); });
    auto timer = std::make_shared<steady_timer>(m_ioService, RECREATE_DELAY);
    timer->async_wait([this, timer](const boost::system::error_code &ec) {
        if (!ec)
            createConnection();
    });
}

void ConnectionPool::createConnection()
{
    auto conn = std::make_shared<Connection>(m_ioService, m_context,
        m_verifyServerCertificate, m_getHandshake, m_onHandshakeResponse,
        std::bind(&ConnectionPool::onMessageReceived, this, _1),
        std::bind(&ConnectionPool::onConnectionReady, this, _1),
        std::bind(&ConnectionPool::onConnectionClosed, this, _1));

    m_connectionsStrand.post([this, conn] { m_connections.emplace(conn); });
    conn->connect(m_host, m_service);
}

ConnectionPool::~ConnectionPool()
{
    m_connectionsStrand.dispatch([this] {
        for (auto &conn : m_connections)
            conn->close();

        m_connections.clear();
    });

    m_ioService.stop();
    m_outbox.abort();

    for (auto &thread : m_workers)
        thread.join();
}

} // namespace communication
} // namespace one
