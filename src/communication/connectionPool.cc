/**
 * @file connectionPool.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "connectionPool.h"

#include "cert/certificateData.h"
#include "etls/utils.h"
#include "exception.h"
#include "logging.h"

#include <asio.hpp>
#include <asio/ssl.hpp>
#include <openssl/ssl.h>

#include <algorithm>
#include <array>
#include <iterator>
#include <tuple>

using namespace std::placeholders;
using namespace std::literals::chrono_literals;

namespace one {
namespace communication {

ConnectionPool::ConnectionPool(const std::size_t connectionsNumber,
    std::string host, const unsigned short port,
    const bool verifyServerCertificate, ConnectionFactory connectionFactory)
    : m_connectionsNumber{connectionsNumber}
    , m_host{std::move(host)}
    , m_port{port}
    , m_verifyServerCertificate{verifyServerCertificate}
    , m_connectionFactory{std::move(connectionFactory)}
{
    LOG_FCALL() << LOG_FARG(connectionsNumber) << LOG_FARG(host)
                << LOG_FARG(port) << LOG_FARG(verifyServerCertificate);

    m_thread = std::thread{[=] {
        LOG_DBG(1) << "Starting ConnectionPool thread";
        etls::utils::nameThread("ConnectionPool");
        m_ioService.run();
    }};
}

void ConnectionPool::connect()
{
    LOG_FCALL();

    m_context->set_options(asio::ssl::context::default_workarounds |
        asio::ssl::context::no_sslv2 | asio::ssl::context::no_sslv3 |
        asio::ssl::context::no_tlsv1 | asio::ssl::context::no_tlsv1_1 |
        asio::ssl::context::single_dh_use);

    m_context->set_default_verify_paths();
    m_context->set_verify_mode(m_verifyServerCertificate
            ? asio::ssl::verify_peer
            : asio::ssl::verify_none);

    SSL_CTX *ssl_ctx = m_context->native_handle();
    auto mode = SSL_CTX_get_session_cache_mode(ssl_ctx) | SSL_SESS_CACHE_CLIENT;

    SSL_CTX_set_session_cache_mode(ssl_ctx, mode);

    if (m_certificateData)
        m_certificateData->initContext(*m_context);

    std::generate_n(
        std::back_inserter(m_connections), m_connectionsNumber, [&] {
            auto connection = m_connectionFactory(m_host, m_port, m_context,
                asio::wrap(m_ioService, m_onMessage),
                std::bind(&ConnectionPool::onConnectionReady, this, _1),
                m_getHandshake, m_onHandshakeResponse, m_onHandshakeDone);

            LOG_DBG(1) << "Establishing connection in connection pool to "
                       << m_host;

            connection->connect();

            return connection;
        });

    m_connected = true;
}

void ConnectionPool::setHandshake(std::function<std::string()> getHandshake,
    std::function<std::error_code(std::string)> onHandshakeResponse,
    std::function<void(std::error_code)> onHandshakeDone)
{
    LOG_FCALL();

    m_getHandshake = std::move(getHandshake);
    m_onHandshakeResponse = std::move(onHandshakeResponse);
    m_onHandshakeDone = std::move(onHandshakeDone);
}

void ConnectionPool::setOnMessageCallback(
    std::function<void(std::string)> onMessage)
{
    LOG_FCALL();

    m_onMessage = std::move(onMessage);
}

void ConnectionPool::setCertificateData(
    std::shared_ptr<cert::CertificateData> certificateData)
{
    LOG_FCALL();

    m_certificateData = std::move(certificateData);
}

void ConnectionPool::send(std::string message, Callback callback, const int)
{
    LOG_FCALL() << LOG_FARG(message.size());

    LOG_DBG(2) << "Attempting to send message of size " << message.size();

    if (!m_connected) {
        return;
    }

    Connection *conn;
    try {
        LOG_DBG(2) << "Waiting for idle connection to become available";
        m_idleConnections.pop(conn);
        LOG_DBG(2) << "Retrieved active connection from connection pool";
    }
    catch (const tbb::user_abort &) {
        // We have aborted the wait by calling stop()
        LOG(ERROR) << "Waiting for connection from connection pool aborted";
        return;
    }

    // There might be a case that the connection has failed between
    // inserting it into ready queue and popping it here; that's ok
    // since connection will fail the send instead of erroring out.
    conn->send(std::move(message), std::move(callback));

    LOG_DBG(1) << "Message sent";
}

void ConnectionPool::onConnectionReady(Connection &conn)
{
    LOG_FCALL();

    LOG_DBG(1) << "Connection established - adding to idle connection pool";

    m_idleConnections.emplace(&conn);
}

ConnectionPool::~ConnectionPool()
{
    LOG_FCALL();

    stop();
}

void ConnectionPool::stop()
{
    LOG_FCALL();

    LOG_DBG(1) << "Stopping connection pool";

    m_connected = false;
    m_connections.clear();
    m_idleConnections.abort();

    if (!m_ioService.stopped())
        m_ioService.stop();

    if (m_thread.joinable())
        m_thread.join();

    LOG_DBG(1) << "Connection pool stopped";
}

} // namespace communication
} // namespace one
