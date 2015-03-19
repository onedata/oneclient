/**
 * @file connection.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "connection.h"

#include "exception.h"
#include "logging.h"

#include <boost/asio.hpp>
#include <openssl/ssl.h>

#include <array>

using namespace std::placeholders;

namespace one {
namespace communication {

Connection::Connection(boost::asio::io_service &ioService,
    boost::asio::ssl::context &context, const bool verifyServerCertificate,
    std::function<std::string()> &getHandshake,
    std::function<bool(std::string)> &onHandshakeResponse,
    std::function<void(std::string)> onMessageReceived,
    std::function<void(std::shared_ptr<Connection>)> onReady,
    std::function<void(std::shared_ptr<Connection>)> onClosed)
    : m_verifyServerCertificate{verifyServerCertificate}
    , m_getHandshake{getHandshake}
    , m_onHandshakeResponse{onHandshakeResponse}
    , m_onMessageReceived{std::move(onMessageReceived)}
    , m_onReady{std::move(onReady)}
    , m_onClosed{std::move(onClosed)}
    , m_strand{ioService}
    , m_socket{ioService, context}
{
}

Connection::~Connection()
{
    m_strand.dispatch([this] {
        if (m_socket.lowest_layer().is_open())
            close();
    });
}

void Connection::send(std::string message, std::promise<void> promise)
{
    send(std::move(message), std::move(promise),
        std::bind(m_onReady, shared_from_this()));
}

void Connection::send(std::string message, std::promise<void> promise,
    std::function<void()> handler)
{
    m_outHeader = htonl(message.size());
    m_outBuffer = std::move(message);
    m_outPromise = std::move(promise);
    m_strand.post(
        std::bind(&Connection::writeOne, shared_from_this(), handler));
}

void Connection::connect(boost::asio::ip::tcp::resolver::iterator endpointIt)
{
    m_strand.dispatch([this, endpointIt] {
        boost::asio::async_connect(
            m_socket.lowest_layer(), endpointIt,
            [ this, t = shared_from_this() ](
                const boost::system::error_code &ec,
                boost::asio::ip::tcp::resolver::iterator) {

                if (ec) {
                    close("Failed to establish TCP connection", ec);
                    return;
                }

                m_socket.lowest_layer().set_option(
                    boost::asio::ip::tcp::no_delay{true});

                m_socket.async_handshake(boost::asio::ssl::stream_base::client,
                    [this, t](const boost::system::error_code &ec) {
                        if (ec) {
                            auto verifyResult =
                                SSL_get_verify_result(m_socket.native_handle());

                            if (verifyResult != 0 &&
                                m_verifyServerCertificate) {
                                close("Server certificate verification failed. "
                                      "OpenSSL error",
                                    ec);
                            }
                            else {
                                close("Failed to perform SSL handshake", ec);
                            }

                            return;
                        }

                        handshake();
                    });
            });
    });
}

void Connection::handshake()
{
    if (!m_getHandshake) {
        readLoop();
        m_onReady(shared_from_this());
        return;
    }

    send(m_getHandshake(), std::promise<void>{}, [this] {
        readOne([this] {
            if (!m_onHandshakeResponse(std::move(m_inBuffer))) {
                LOG(WARNING)
                    << "Handshake handler asked to close the connection";
                close();
                return;
            }

            m_onReady(shared_from_this());
            readLoop();
        });
    });
}

void Connection::readLoop()
{
    readOne([this] {
        m_onMessageReceived(std::move(m_inBuffer));
        readLoop();
    });
}

void Connection::readOne(std::function<void()> handler)
{
    boost::asio::async_read(m_socket, headerToBuffer(m_inHeader),
        m_strand.wrap(
            [ this, t = shared_from_this(), handler = std::move(handler) ](
                const boost::system::error_code &ec, size_t) {

                if (ec) {
                    close("Failed to read message header", ec);
                    return;
                }

                const auto messageSize = ntohl(m_inHeader);
                m_inBuffer.resize(messageSize);

                boost::asio::async_read(m_socket,
                    boost::asio::mutable_buffers_1{
                        &m_inBuffer[0], m_inBuffer.size()},
                    m_strand.wrap([ this, t, handler = std::move(handler) ](
                        const boost::system::error_code &ec, size_t) {
                        if (ec) {
                            close("Failed to read message body", ec);
                            return;
                        }

                        handler();
                    }));
            }));
}

void Connection::writeOne(std::function<void()> handler)
{
    std::array<boost::asio::const_buffer, 2> compositeBuffer{
        {headerToBuffer(m_outHeader), boost::asio::buffer(m_outBuffer)}};

    boost::asio::async_write(m_socket, compositeBuffer,
        m_strand.wrap(
            [ this, t = shared_from_this(), handler = std::move(handler) ](
                const boost::system::error_code &ec, size_t) {

                if (ec) {
                    auto msg = close("Failed to write message", ec);
                    auto e = std::make_exception_ptr(SendError{msg});
                    m_outPromise.set_exception(e);
                    return;
                }

                m_outPromise.set_value();
                handler();
            }));
}

void Connection::close()
{
    m_strand.dispatch([this] {
        boost::system::error_code ec;
        auto logOnError = [&ec](auto what) {
            if (ec)
                LOG(WARNING) << what << ": " << ec.message();
        };

        m_socket.shutdown(ec);
        logOnError("Failed to shutdown SSL connection layer");

        m_socket.lowest_layer().shutdown(
            boost::asio::ip::tcp::socket::shutdown_both, ec);
        logOnError("Failed to shutdown TCP connection");

        m_socket.lowest_layer().close(ec);
        logOnError("Failed to cleanly close TCP socket");

        m_onClosed(shared_from_this());
    });
}

std::string Connection::close(
    std::string what, const boost::system::error_code &ec)
{
    auto msg = what + ": " + ec.message();
    LOG(ERROR) << msg;
    close();
    return msg;
}

boost::asio::mutable_buffers_1 Connection::headerToBuffer(std::uint32_t &header)
{
    return {static_cast<void *>(&header), sizeof(header)};
}

} // namespace communication
} // namespace one
