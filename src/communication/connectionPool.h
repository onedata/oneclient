/**
 * @file connectionPool.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CONNECTION_POOL_H
#define HELPERS_COMMUNICATION_CONNECTION_POOL_H

#include "ioServiceExecutor.h"

#include <boost/asio/io_service.hpp>
#include <boost/asio/strand.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl/context.hpp>
#include <boost/asio/ssl/stream.hpp>
#include <boost/thread/future.hpp>
#include <tbb/concurrent_queue.h>

#include <functional>
#include <memory>
#include <string>
#include <queue>
#include <vector>
#include <thread>
#include <tuple>
#include <unordered_set>

namespace one {
namespace communication {

namespace cert {
class CertificateData;
}

class Connection;

/**
 * A @c ConnectionPool is responsible for managing instances of @c Connection.
 * It provides a facade for the connections, ensuring that outside entities
 * do not interact with connections directly.
 */
class ConnectionPool {
    using SendTask = std::tuple<std::string, boost::promise<void>>;

public:
    enum class ErrorPolicy { ignore, propagate };

    /**
     * A reference to @c *this typed as a @c ConnectionPool.
     */
    ConnectionPool &connectionPool = *this;

    /**
     * Constructor.
     * @param connectionsNumber Number of connections that should be maintained
     * by this pool.
     * @param host Hostname of the remote endpoint.
     * @param service Name of well-known service provided by the remote
     * endpoint, or a port number.
     * @param verifyServerCertificate Specifies whether to verify server's
     * SSL certificate.
     * @param certificateData Certificate data to use for SSL authentication.
     */
    ConnectionPool(const unsigned int connectionsNumber, std::string host,
        std::string service, const bool verifyServerCertificate,
        ErrorPolicy errorPolicy = ErrorPolicy::ignore);

    /**
     * Creates connections and threads that will work for them.
     * May throw a connection-related exception.
     * @note This method is separated from the constructor so that the
     * initialization can be augmented by other communication layers.
     */
    void connect();

    /**
     * Sets handshake-related functions.
     * The handshake functions are passed down to connections and used on
     * initialization of each TCP connection.
     * @param getHandshake A function that returns a handshake to send through
     * connections.
     * @param onHandshakeResponse A function that takes a handshake response.
     * @note This method is separated from constructor so that the handshake
     * messages can be translated by other communication layers.
     */
    void setHandshake(std::function<std::string()> getHandshake,
        std::function<bool(std::string)> onHandshakeResponse);

    /**
     * Sets a function to handle received messages.
     * @param onMessage The received message.
     */
    void setOnMessageCallback(std::function<void(std::string)> onMessage);

    /**
     * Sets certificate data to be used to authorize the client.
     * @param certificateData The certificate data to set.
     */
    void setCertificateData(
        std::shared_ptr<cert::CertificateData> certificateData);

    /**
     * Sends a message through one of the managed connections.
     * @param message The message to send.
     * @return A future fulfilled when the message is sent or (with an
     * exception) when an error occured.
     */
    boost::future<void> send(std::string message, const int = int{});

    /**
     * Destructor.
     * Stops the underlying Boost::Asio endpoint and the worker thread and
     * closes maintained connections.
     */
    virtual ~ConnectionPool();

private:
    void createConnection();
    void onMessageReceived(std::string message);
    void onConnectionReady(std::shared_ptr<Connection> conn);
    void onConnectionClosed(
        std::shared_ptr<Connection> conn, boost::exception_ptr exception);

    const unsigned int m_connectionsNumber;
    std::string m_host;
    std::string m_service;
    const bool m_verifyServerCertificate;
    ErrorPolicy m_errorPolicy;
    std::shared_ptr<const cert::CertificateData> m_certificateData;

    std::function<std::string()> m_getHandshake;
    std::function<bool(std::string)> m_onHandshakeResponse;
    std::function<void(std::string)> m_onMessage = [](auto) {};

    boost::asio::io_service m_ioService;
    boost::asio::io_service::work m_idleWork;
    boost::asio::io_service::strand m_blockingStrand;
    boost::asio::io_service::strand m_connectionsStrand;

    std::vector<std::thread> m_workers;
    boost::asio::ssl::context m_context;

    tbb::concurrent_bounded_queue<std::shared_ptr<SendTask>> m_outbox;
    tbb::concurrent_queue<std::shared_ptr<SendTask>> m_rejects;
    std::unordered_set<std::shared_ptr<Connection>> m_connections;

protected:
    /**
     * A thread executor working on top of this pool's @c io_service.
     * @note Defined after other variables as it depends on private ioService.
     */
    IoServiceExecutor m_ioServiceExecutor;
};

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_CONNECTION_POOL_H
