/**
 * @file connection.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_WEBSOCKET_CONNECTION_H
#define HELPERS_COMMUNICATION_WEBSOCKET_CONNECTION_H

#include "communication/connection.h"

#include "communication/exception.h"

#include <websocketpp/client.hpp>
#include <websocketpp/config/asio_client.hpp>
#include <websocketpp/connection.hpp>

#include <exception>
#include <memory>
#include <string>
#include <unordered_map>

extern template class websocketpp::client<websocketpp::config::asio_tls_client>;

namespace one {
namespace communication {

class CertificateData;

namespace websocket {

/**
 * An @c one::communication::ConnectionError specialization for connection
 * errors occuring due to invalid server certificate.
 */
class InvalidServerCertificate : public ConnectionError {
public:
    using ConnectionError::ConnectionError;
};

/**
 * The Connection class is a @c one::communication::Connection
 * specialization for TLS WebSocket++ based connections.
 */
class Connection : public ::one::communication::Connection {
    using config_type = websocketpp::config::asio_tls_client;
    using endpoint_type = websocketpp::client<config_type>;
    using connection_ptr = endpoint_type::connection_ptr;
    using message_ptr = config_type::message_type::ptr;

public:
    /**
     * Constructor.
     * @param onMessageCallback Callback to be called on received message.
     * @param onFailCallback Callback to be called on connection open failure.
     * @param onOpenCallback Callback to be called on connection open.
     * @param onErrorCallback Callback to be called on open connection's error.
     * @param endpoint A reference to an ASIO endpoint.
     * @param uri Server's URI to connect to.
     * @param additionalHeaders Additional HTTP headers to use for the
     * connection.
     * @param certificateData Certificate data to use for SSL authentication.
     * @param verifyServerCertificate Determines whether to verify server's
     * certificate.
     */
    Connection(
        std::function<void(const std::string &)> onMessageCallback,
        std::function<void(::one::communication::Connection &,
                           std::exception_ptr)> onFailCallback,
        std::function<void(::one::communication::Connection &)> onOpenCallback,
        std::function<void(::one::communication::Connection &)> onErrorCallback,
        endpoint_type &endpoint, const std::string &uri,
        const std::unordered_map<std::string, std::string> &additionalHeaders,
        std::shared_ptr<const CertificateData> certificateData,
        const bool verifyServerCertificate);

    /**
     * Destructor.
     * Closes the connection.
     */
    ~Connection();

    /**
     * Sends a message through the connection.
     * @param payload The message to send.
     */
    void send(const std::string &payload) override;

private:
    /**
     * @defgroup WebSocket++ handlers.
     * @see websocketpp::transport::asio::tls_socket::connection
     * @{
     */
    void onMessage(message_ptr msg);
    void onOpen();
    void onClose();
    void onFail();
    bool onPing(std::string);
    void onPong(std::string);
    void onPongTimeout(std::string);
    void onInterrupt();
    /**@}*/

    endpoint_type &m_endpoint;
    const std::shared_ptr<const CertificateData> m_certificateData;
    const bool m_verifyServerCertificate;
    websocketpp::connection_hdl m_connection;
};

} // namespace websocket
} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_WEBSOCKET_CONNECTION_H
