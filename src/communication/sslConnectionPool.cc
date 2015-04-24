/**
 * @file sslConnectionPool.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/sslConnectionPool.h"

#include "communication/certificateData.h"
#include "communication/exception.h"
#include "communication/sslConnection.h"
#include "logging.h"

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

SSLConnectionPool::SSLConnectionPool(const unsigned int connectionsNumber,
    std::string host, std::string port,
    std::shared_ptr<const CertificateData> certificateData,
    const bool verifyServerCertificate,
    std::function<void(std::vector<char>)> onMessage)
    : m_certificateData{std::move(certificateData)}
    , m_verifyServerCertificate{verifyServerCertificate}
    , m_idleWork{m_ioService}
    , m_blockingStrand{m_ioService}
    , m_connectionsStrand{m_ioService}
    , m_context{boost::asio::ssl::context::tlsv12_client}
    , m_onMessage{std::move(onMessage)}
{
    m_outbox.set_capacity(OUTBOX_SIZE);

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

    boost::asio::ip::tcp::resolver resolver{m_ioService};
    boost::asio::ip::tcp::resolver::query query{host, port};

    try {
        m_endpointIterator = resolver.resolve(query);

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

    for (auto i = 0u; i < connectionsNumber; ++i)
        createConnection();
}

std::future<void> SSLConnectionPool::send(std::vector<char> message)
{
    std::promise<void> promise;
    auto future = promise.get_future();
    auto data =
        std::make_shared<std::tuple<std::vector<char>, std::promise<void>>>(
            std::forward_as_tuple(std::move(message), std::move(promise)));

    m_outbox.emplace(std::move(data));
    return future;
}

void SSLConnectionPool::onMessageReceived(std::vector<char> message)
{
    m_ioService.post([ this, message = std::move(message) ] {
        m_onMessage(message);
    });
}

void SSLConnectionPool::onConnectionReady(std::shared_ptr<SSLConnection> conn)
{
    m_blockingStrand.post([ this, c = std::weak_ptr<SSLConnection>{conn} ] {
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
void SSLConnectionPool::onConnectionClosed(std::shared_ptr<SSLConnection> conn)
{
    m_connectionsStrand.post([this, conn] { m_connections.erase(conn); });
    auto timer = std::make_shared<steady_timer>(m_ioService, RECREATE_DELAY);
    timer->async_wait([this, timer](const boost::system::error_code &ec) {
        if (!ec)
            createConnection();
    });
}

void SSLConnectionPool::createConnection()
{
    auto conn = std::make_shared<SSLConnection>(m_ioService, m_context,
        m_verifyServerCertificate,
        std::bind(&SSLConnectionPool::onMessageReceived, this, _1),
        std::bind(&SSLConnectionPool::onConnectionReady, this, _1),
        std::bind(&SSLConnectionPool::onConnectionClosed, this, _1));

    m_connectionsStrand.post([this, conn] { m_connections.emplace(conn); });
    conn->start(m_endpointIterator);
}

SSLConnectionPool::~SSLConnectionPool()
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
