/**
 * @file connection.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CONNECTION_H
#define HELPERS_COMMUNICATION_CONNECTION_H

#include <boost/asio/io_service.hpp>
#include <boost/asio/ssl.hpp>
#include <boost/asio/strand.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/thread/future.hpp>

#include <cstdint>
#include <functional>
#include <future>
#include <memory>
#include <queue>
#include <vector>
#include <tuple>

namespace one {
namespace communication {

/**
 * @c Connection class represents a single TCP/TLS connection between the
 * client and a remote endpoint.
 * The Connection uses @c shared_from_this() to hold a temporary ownership of
 * itself; i.e. it ensures that the object will continue to exist while
 * a callback is in progress or any operations are scheduled on the object.
 *
 * On destruction, parent object is expected to call @c close() and reset
 * @c io_service associated with the object. All scheduled callbacks are dropped
 * along with associated shared pointers when the service is reset. If a
 * callback was already in progress, it won't be able to schedule new socket
 * operations as the socket is closed, and thus all shared pointers will
 * eventually be dropped.
 *
 * A @c Connection object can optionally send a custom handshake message
 * immediately after establishing a secure connection to the server. The object
 * then waits for a handshake response (and passes it up to
 * @c onHandshakeResponse callback) before indicating that it is ready to send
 * data. The @c onHandshakeResponse callback can decide to close the connection
 * by returning false.
 *
 * Aside from handshake-related callbacks, there are three main events that
 * a @c Connection instance can emit: @c onReady, @c onClosed and
 * @c onMessageReceived.
 * - @c onReady indicates that the @c Connection object is ready to send next
 *   message. Sending message before @c onReady is emitted is illegal, as is
 *   sending multiple messages after receiving a single @c onReady.
 * - @c onClosed is emitted when the connection has been closed, whether as
 *   a result of an error or through normal operation. The parent is expected
 *   to drop any references to the @c Connection instance, as it's no longer
 *   in a valid state.
 * - @c onMessageReceived is emitted when a message from the remote endpoint has
 *   been received.
 *
 * No callback shall perform a blocking or computationally-heavy operation, as
 * those would take up resources dedicated to managing connections.
 */
class Connection : public std::enable_shared_from_this<Connection> {
public:
    /**
     * Constructor.
     * @param ioService A reference to io_service that will handle this object's
     * operations, including but not limited to a Boost::Asio socket.
     * @param context Boost::Asio SSL context used to establish a secure
     * connection with the server.
     * @param verifyServerCertificate Specifies whether to verify server's
     * SSL certificate.
     * @param getHandshake A reference to a function returning a handshake
     * message.
     * @param onHandshakeResponse A reference to a callback taking a handshake
     * response.
     * @param onMessageReceived A callback to call when onMessageReceived event
     * is emitted.
     * @param onReady A callback to call when onReady event is emitted.
     * @param onClosed A callback to call when onClosed event is emitted.
     */
    Connection(boost::asio::io_service &ioService,
        boost::asio::ssl::context &context, const bool verifyServerCertificate,
        std::function<std::string()> &getHandshake,
        std::function<bool(std::string)> &onHandshakeResponse,
        std::function<void(std::string)> onMessageReceived,
        std::function<void(std::shared_ptr<Connection>)> onReady,
        std::function<void(std::shared_ptr<Connection>)> onClosed);

    /**
     * Destructor.
     * Closes the connection if it's not already closed.
     */
    ~Connection();

    /**
     * Starts an asynchronous chain of events, from establishing a TCP
     * connection, through performing an SSL handshake, to performing a custom
     * handshake set in the constructor, to starting a read loop and emitting
     * an @c onReady event.
     * @param host The hostname/ip address to connect to.
     * @param service The service/port to connect to.
     * @note @c connect is separated from the constructor because calling
     * @c shared_from_this() is invalid in constructors and to avoid race
     * conditions from events (e.g. @c onClosed is emitted before the connection
     * is added to an array in a parent object).
     */
    void connect(const std::string &host, const std::string &service);

    /**
     * Sends a message, fulfilling a promise after the message is sent.
     * The promise is fulfilled with an exception when appropriate.
     * @param message The message to send.
     * @param promise The promise to fultill.
     * @note @c send can only be called after @c onReady has been emitted.
     */
    void send(std::string message, boost::promise<void> promise);

    /**
     * Gracefully closes the underlying connection.
     * After calling the method. the @c Connection object is no longer in a
     * useful state. and should be destroyed.
     */
    void close();

    Connection(const Connection &) = delete;
    Connection(Connection &&) = delete;
    Connection &operator=(const Connection &) = delete;
    Connection &operator=(Connection &&) = delete;

private:
    void send(std::string message, boost::promise<void> promise,
        std::function<void()> handler);

    void handshake();
    void readOne(std::function<void()> handler);
    void writeOne(std::function<void()> handler);
    void readLoop();
    std::string close(std::string what, const boost::system::error_code &ec);
    boost::asio::mutable_buffers_1 headerToBuffer(std::uint32_t &header);

    const bool m_verifyServerCertificate;

    std::function<std::string()> &m_getHandshake;
    std::function<bool(std::string)> &m_onHandshakeResponse;

    std::function<void(std::string)> m_onMessageReceived;
    std::function<void(std::shared_ptr<Connection>)> m_onReady;
    std::function<void(std::shared_ptr<Connection>)> m_onClosed;

    boost::asio::ip::tcp::resolver m_resolver;
    boost::asio::io_service::strand m_strand;
    boost::asio::ssl::stream<boost::asio::ip::tcp::socket> m_socket;

    std::uint32_t m_inHeader;
    std::string m_inBuffer;

    std::uint32_t m_outHeader;
    std::string m_outBuffer;
    boost::promise<void> m_outPromise;
};

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_CONNECTION_H
