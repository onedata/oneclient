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
#include <boost/asio/spawn.hpp>
#include <openssl/ssl.h>

#include <algorithm>
#include <array>
#include <vector>
#include <random>

using namespace std::placeholders;

namespace one {
namespace communication {

template <>
std::string Connection::close<void>(
    std::string what, const boost::system::error_code &ec)
{
    auto msg = what + (ec ? ": " + ec.message() : std::string{});
    LOG(ERROR) << msg;
    close();
    return msg;
}

template <class Ex>
std::string Connection::close(
    std::string what, const boost::system::error_code &ec)
{
    auto msg = what + (ec ? ": " + ec.message() : std::string{});
    LOG(ERROR) << msg;
    close(boost::copy_exception(Ex{msg}));
    return msg;
}

Connection::Connection(boost::asio::io_service &ioService,
    boost::asio::ssl::context &context, const bool verifyServerCertificate,
    std::function<std::string()> &getHandshake,
    std::function<bool(std::string)> &onHandshakeResponse,
    std::function<void(std::string)> onMessageReceived,
    std::function<void(std::shared_ptr<Connection>)> onReady,
    std::function<void(std::shared_ptr<Connection>, boost::exception_ptr)>
        onClosed)
    : m_verifyServerCertificate{verifyServerCertificate}
    , m_getHandshake{getHandshake}
    , m_onHandshakeResponse{onHandshakeResponse}
    , m_onMessageReceived{std::move(onMessageReceived)}
    , m_onReady{std::move(onReady)}
    , m_onClosed{std::move(onClosed)}
    , m_resolver{ioService}
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

void Connection::send(std::string message, boost::promise<void> promise)
{
    prepareBuffer(std::move(message), std::move(promise));
    boost::asio::spawn(m_strand, [t = shared_from_this()](auto yield) mutable {
        boost::system::error_code ec;

        t->send(ec, yield);
        if (ec) {
            auto msg = t->close("failed to write message", ec);
            auto e = std::make_exception_ptr(SendError{msg});
            t->m_outPromise.set_exception(e);
        }
        else {
            t->m_outPromise.set_value();
        }
    });
}

void Connection::send(
    boost::system::error_code &ec, boost::asio::yield_context yield)
{
    std::array<boost::asio::const_buffer, 2> buffer{
        headerToBuffer(m_outHeader), boost::asio::buffer(m_outBuffer)};

    boost::asio::async_write(m_socket, buffer, yield[ec]);
}

void Connection::prepareBuffer(
    std::string message, boost::promise<void> promise)
{
    m_outHeader = htonl(message.size());
    m_outBuffer = std::move(message);
    m_outPromise = std::move(promise);
}

void Connection::connect(const std::string &host, const std::string &service)
{
    boost::asio::spawn(
        m_strand, [ =, t = shared_from_this() ](auto yield) mutable {
            t->connect(std::move(host), std::move(service), yield);
        });
}

void Connection::connect(
    std::string host, std::string service, boost::asio::yield_context yield)
{
    boost::system::error_code ec;
    auto endpoints = resolve(std::move(host), std::move(service), ec, yield);

    if (ec) {
        close<ConnectionError>(
            "failed to resolve host: " + host + " and service: " + service, ec);
        return;
    }

    boost::asio::async_connect(
        m_socket.lowest_layer(), endpoints.begin(), endpoints.end(), yield[ec]);

    if (ec) {
        close<ConnectionError>("failed to establish TCP connection", ec);
        return;
    }

    m_socket.lowest_layer().set_option(boost::asio::ip::tcp::no_delay{true});
    m_socket.async_handshake(boost::asio::ssl::stream_base::client, yield[ec]);

    if (ec) {
        auto verifyResult = SSL_get_verify_result(m_socket.native_handle());

        if (verifyResult != 0 && m_verifyServerCertificate) {
            close<InvalidServerCertificate>("server certificate verification "
                                            "failed. OpenSSL error " +
                    std::to_string(verifyResult),
                ec);
        }
        else
            close<ConnectionError>("failed to perform SSL handshake", ec);

        return;
    }

    handshake(yield);
}

std::vector<boost::asio::ip::basic_resolver_entry<boost::asio::ip::tcp>>
Connection::resolve(std::string host, std::string service,
    boost::system::error_code &ec, boost::asio::yield_context yield)
{
    boost::asio::ip::tcp::resolver::query query{
        std::move(host), std::move(service)};
    auto iterator = m_resolver.async_resolve(query, yield[ec]);

    if (ec)
        return {};

    std::vector<decltype(iterator)::value_type> endpoints;
    std::move(iterator, decltype(iterator){}, std::back_inserter(endpoints));

    std::random_device rd;
    std::default_random_engine engine{rd()};
    std::shuffle(endpoints.begin(), endpoints.end(), engine);

    return endpoints;
}

void Connection::handshake(boost::asio::yield_context yield)
{
    if (m_getHandshake) {
        boost::system::error_code ec;

        prepareBuffer(m_getHandshake(), {});
        send(ec, yield);
        if (ec) {
            close<ConnectionError>("failed to send handshake message", ec);
            return;
        }

        receive(ec, yield);
        if (ec) {
            close<ConnectionError>("failed to receive handshake response", ec);
            return;
        }

        if (!m_onHandshakeResponse(std::move(m_inBuffer))) {
            close<ConnectionError>("handshake aborted by the client");
            return;
        }
    }

    m_onReady(shared_from_this());
    readLoop(yield);
}

void Connection::readLoop(boost::asio::yield_context yield)
{
    boost::system::error_code ec;
    while (true) {
        receive(ec, yield);
        if (ec) {
            close("failed to receive a message", ec);
            return;
        }

        m_onMessageReceived(std::move(m_inBuffer));
    }
}

void Connection::receive(
    boost::system::error_code &ec, boost::asio::yield_context yield)
{
    boost::asio::async_read(m_socket, headerToBuffer(m_inHeader), yield[ec]);

    if (ec)
        return;

    const auto messageSize = ntohl(m_inHeader);
    m_inBuffer.resize(messageSize);

    boost::asio::mutable_buffers_1 buffer{&m_inBuffer[0], m_inBuffer.size()};
    boost::asio::async_read(m_socket, buffer, yield[ec]);
}

void Connection::close(boost::exception_ptr exception)
{
    m_strand.dispatch([ this, exception = std::move(exception) ] {
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

        m_onClosed(shared_from_this(), std::move(exception));
    });
}

boost::asio::mutable_buffers_1 Connection::headerToBuffer(std::uint32_t &header)
{
    return {static_cast<void *>(&header), sizeof(header)};
}

} // namespace communication
} // namespace one
