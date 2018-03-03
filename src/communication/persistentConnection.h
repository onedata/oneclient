/**
 * @file persistentConnection.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_PERSISTENT_CONNECTION_H
#define HELPERS_COMMUNICATION_PERSISTENT_CONNECTION_H

#include "connection.h"

#include "etls/tlsApplication.h"
#include "etls/tlsSocket.h"

#include <asio/buffer.hpp>
#include <asio/ssl/context.hpp>
#include <asio/steady_timer.hpp>

#include <array>
#include <chrono>
#include <cstdint>
#include <functional>
#include <memory>
#include <string>
#include <system_error>

namespace one {
namespace communication {

static constexpr std::chrono::seconds RECREATE_DELAY{2};

/**
 * @c PersistentConnection class represents a single TCP/TLS connection between
 * the client and a remote endpoint.
 *
 * A @c PersistentConnection object can optionally send a custom handshake
 * message immediately after establishing a secure connection to the server. The
 * object then waits for a handshake response (and passes it up to
 * @c onHandshakeResponse callback) before indicating that it is ready to send
 * data. The @c onHandshakeResponse callback can decide to close the connection
 * by returning false.
 *
 * Aside from handshake-related callbacks, there are two main events that
 * a @c PersistentConnection instance can emit: @c onReady and
 * @c onMessageReceived.
 * - @c onReady indicates that the @c PersistentConnection object is ready to
 *   send next message. Sending message before @c onReady is emitted is illegal,
 *   as is sending multiple messages after receiving a single @c onReady.
 * - @c onMessageReceived is emitted when a message from the remote endpoint has
 *   been received.
 *
 * No callback shall perform a blocking or computationally-heavy operation, as
 * those would take up resources dedicated to managing connections.
 */
class PersistentConnection : public Connection {
public:
    using Connection::Callback;

    /**
     * Constructor.
     * @param host Hostname of the remote endpoint.
     * @param port Port number of the remote endpoint.
     * @param context asio SSL context used to establish a secure connection
     * with the server.
     * @param onMessage Callback called with a message received from the server.
     * @param onReady Callback called when the connection is ready to send data.
     * @param getHandshake Function returning a handshake message.
     * @param onHandshakeResponse Callback called with a handshake response
     * received from the server.
     * @param onHandshakeDone Callback called when handshake is done with
     * success or error.
     */
    PersistentConnection(std::string host, const unsigned short port,
        std::shared_ptr<asio::ssl::context> context,
        std::function<void(std::string)> onMessage,
        std::function<void(PersistentConnection &)> onReady,
        std::function<std::string()> getHandshake = {},
        std::function<std::error_code(std::string)> onHandshakeResponse = {},
        std::function<void(std::error_code)> onHandshakeDone = {});

    /**
     * Destructor.
     * Attempts to gracefully close the managed connection.
     */
    ~PersistentConnection();

    /**
     * Sends a message through the managed connection.
     * @param message The message to send.
     * @param callback Callback called when the message is successfuly sent or
     * failed with error.
     */
    void send(std::string message, Callback callback) override;

    /**
     * Starts the managed connection.
     */
    void connect() override;

    /**
     * Invokes the protocol upgrade HTTP request
     */
    void upgrade() override;

    PersistentConnection(const PersistentConnection &) = delete;
    PersistentConnection(PersistentConnection &&) = delete;
    PersistentConnection &operator=(const PersistentConnection &) = delete;
    PersistentConnection &operator=(PersistentConnection &&) = delete;

private:
    static const std::string CLPROTO_UPGRADE_ENDPOINT;
    static const std::string CLPROTO_UPGRADE_RESPONSE_STATUS;

    void onConnect();
    void onUpgradeRequestSent();
    void onUpgradeResponseReceived();
    void onHandshakeSent();
    void onHandshakeReceived();
    void onSent();
    void onError(const std::error_code &ec);
    void readLoop();

    void close();
    void notify(const std::error_code &ec = {});
    void start();

    etls::TLSSocket::Ptr getSocket();

    template <typename... Args, typename SF>
    etls::Callback<Args...> createCallback(SF &&onSuccess);
    template <typename SF> void asyncRead(SF &&onSuccess);
    template <typename SF>
    void asyncReadRawUntil(std::string delimiter, SF &&onSuccess);
    std::array<asio::const_buffer, 2> prepareOutBuffer(std::string message);
    std::array<asio::const_buffer, 1> prepareRawOutBuffer(std::string message);
    asio::mutable_buffers_1 headerToBuffer(std::uint32_t &header);

    std::string m_host;
    const unsigned short m_port;
    std::shared_ptr<asio::ssl::context> m_context;
    std::function<void(std::string)> m_onMessage;
    std::function<void(PersistentConnection &)> m_onReady;
    std::function<std::string()> m_getHandshake;
    std::function<std::error_code(std::string)> m_onHandshakeResponse;
    std::function<void(std::error_code)> m_onHandshakeDone;

    Callback m_callback;

    etls::TLSSocket::Ptr m_socket;
    etls::TLSApplication m_app{1};
    asio::steady_timer m_recreateTimer{m_app.ioService()};
    std::atomic<bool> m_connected{false};
    std::atomic<int> m_connectionId{0};

    std::uint32_t m_inHeader;
    std::string m_inData;
    std::uint32_t m_outHeader;
    std::string m_outData;
};

std::unique_ptr<Connection> createConnection(std::string host,
    const unsigned short port, std::shared_ptr<asio::ssl::context> context,
    std::function<void(std::string)> onMessage,
    std::function<void(Connection &)> onReady,
    std::function<std::string()> getHandshake = {},
    std::function<std::error_code(std::string)> onHandshakeResponse = {},
    std::function<void(std::error_code)> onHandshakeDone = {});

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_PERSISTENT_CONNECTION_H
