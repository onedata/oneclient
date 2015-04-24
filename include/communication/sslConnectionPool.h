/**
 * @file sslConnectionPool.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_SSL_CONNECTION_POOL_H
#define HELPERS_COMMUNICATION_SSL_CONNECTION_POOL_H

#include "communication/connectionPool.h"

#include <boost/asio/io_service.hpp>
#include <boost/asio/strand.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl/context.hpp>
#include <boost/asio/ssl/stream.hpp>
#include <tbb/concurrent_queue.h>

#include <functional>
#include <future>
#include <memory>
#include <string>
#include <queue>
#include <vector>
#include <thread>
#include <tuple>
#include <unordered_set>

namespace one {
namespace communication {

class CertificateData;
class SSLConnection;

/**
 * A @c one::communication::ConnectionPool specialization for managing
 * SSL based connections.
 */
class SSLConnectionPool {
    using SendTask = std::tuple<std::vector<char>, std::promise<void>>;

public:
    /**
     * Constructor.
     * Starts an underlying ASIO endpoint with a worker thread.
     * @param connectionsNumber Number of connections that should be maintained
     * by this pool.
     * @param host Server's hostname to connect to.
     * @param port Server's port to connect to.
     * @param certificateData Certificate data to use for SSL authentication.
     * @param verifyServerCertificate Determines whether to verify server's
     * certificate.
     */
    SSLConnectionPool(const unsigned int connectionsNumber, std::string host,
        std::string port,
        std::shared_ptr<const CertificateData> certificateData,
        const bool verifyServerCertificate,
        std::function<void(std::vector<char>)> onMessage);

    std::future<void> send(std::vector<char> message);

    /**
     * Destructor.
     * Stops the underlying ASIO endpoint and the worker thread and closes
     * maintained connections.
     */
    ~SSLConnectionPool();

private:
    void createConnection();
    void onMessageReceived(std::vector<char> message);
    void onConnectionReady(std::shared_ptr<SSLConnection> conn);
    void onConnectionClosed(std::shared_ptr<SSLConnection> conn);

    std::shared_ptr<const CertificateData> m_certificateData;
    const bool m_verifyServerCertificate;
    boost::asio::io_service m_ioService;
    boost::asio::io_service::work m_idleWork;
    boost::asio::io_service::strand m_blockingStrand;
    boost::asio::io_service::strand m_connectionsStrand;
    std::vector<std::thread> m_workers;
    boost::asio::ip::tcp::resolver::iterator m_endpointIterator;
    tbb::concurrent_bounded_queue<std::shared_ptr<SendTask>> m_outbox;
    tbb::concurrent_queue<std::shared_ptr<SendTask>> m_rejects;
    std::unordered_set<std::shared_ptr<SSLConnection>> m_connections;
    boost::asio::ssl::context m_context;
    std::function<void(std::vector<char>)> m_onMessage;
};

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_SSL_CONNECTION_POOL_H
