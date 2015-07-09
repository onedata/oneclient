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

#include <asio.hpp>
#include <openssl/ssl.h>

#include <algorithm>
#include <array>
#include <vector>
#include <random>
#include <tuple>

using namespace std::placeholders;
using namespace std::literals;

namespace one {
namespace communication {

Connection::Connection(asio::io_service &ioService, asio::ssl::context &context,
    const bool verifyServerCertificate,
    std::function<std::string()> &getHandshake,
    std::function<std::error_code(std::string)> &onHandshakeResponse,
    std::function<void(std::string)> onMessageReceived,
    std::function<void(Ptr, const std::error_code &)> onClosed)
    : m_verifyServerCertificate{verifyServerCertificate}
    , m_getHandshake{getHandshake}
    , m_onHandshakeResponse{onHandshakeResponse}
    , m_onMessageReceived{std::move(onMessageReceived)}
    , m_onClosed{std::move(onClosed)}
    , m_resolver{ioService}
    , m_strand{ioService}
    , m_socket{ioService, context}
{
}

Connection::~Connection()
{
    asio::dispatch(m_strand, [this] {
        if (m_socket.lowest_layer().is_open()) {
            std::error_code ec;
            close(ec);
        }
    });
}

void Connection::send(std::string message, Callback callback)
{
    asio::post(m_strand, [
        this,
        self = shared_from_this(),
        message = std::move(message),
        callback = std::move(callback)
    ]() mutable {
        auto buffer = prepareOutBuffer(std::move(message));

        asio::async_write(m_socket, buffer, asio::wrap(m_strand, [
            this,
            self = std::move(self),
            callback = std::move(callback)
        ](const std::error_code ec, std::size_t) {
            if (ec)
                close(ec);

            callback(ec, std::move(self));
        }));
    });
}

std::array<asio::const_buffer, 2> Connection::prepareOutBuffer(
    std::string message)
{
    m_outHeader = htonl(message.size());
    m_outData = std::move(message);
    return {{headerToBuffer(m_outHeader), asio::buffer(m_outData)}};
}

void Connection::connect(
    std::string host, std::string service, std::function<void(Ptr)> callback)
{
    auto wrappedCallback = [ =, callback = std::move(callback) ](
        Ptr ptr) mutable
    {
        readLoop();
        callback(ptr);
    };

    asio::post(m_strand, [
        this,
        self = shared_from_this(),
        host = std::move(host),
        service = std::move(service),
        callback = std::move(wrappedCallback)
    ]() mutable {
        asio::ip::tcp::resolver::query query{
            std::move(host), std::move(service)};

        this->m_resolver.async_resolve(query, asio::wrap(m_strand, [
            this,
            self = std::move(self),
            callback = std::move(callback)
        ](auto &ec, auto iterator) mutable {

            this->onResolve(ec, std::move(iterator), std::move(callback));

        }));
    });
}

void Connection::onResolve(const std::error_code &ec,
    asio::ip::basic_resolver_iterator<asio::ip::tcp> iterator,
    std::function<void(Ptr)> callback)
{
    if (ec) {
        close(ec);
        return;
    }

    auto entries = shuffleEndpoints(iterator);
#ifndef NDEBUG
    DLOG(INFO) << "Host resolved as:";
    for (auto &entry : entries)
        DLOG(INFO) << "\t" << entry.endpoint().address().to_string();
#endif

    asio::async_connect(m_socket.lowest_layer(), entries.begin(), entries.end(),
        asio::wrap(m_strand, [
            this,
            self = shared_from_this(),
            callback = std::move(callback)
        ](auto &ec, auto) mutable {
            this->onConnect(ec, std::move(callback));
        }));
}

void Connection::onConnect(
    const std::error_code &ec, std::function<void(Ptr)> callback)
{
    if (ec) {
        close(ec);
        return;
    }

    m_socket.lowest_layer().set_option(asio::ip::tcp::no_delay{true});

    m_socket.async_handshake(
        asio::ssl::stream_base::client, asio::wrap(m_strand, [
            this,
            self = shared_from_this(),
            callback = std::move(callback)
        ](auto &ec) mutable {
            this->onTLSHandshake(ec, std::move(callback));
        }));
}

void Connection::onTLSHandshake(
    const std::error_code &ec, std::function<void(Ptr)> callback)
{
    /// @todo certificate verification
    //        auto verifyResult =
    //        SSL_get_verify_result(m_socket.native_handle());

    //        if (verifyResult != 0 && m_verifyServerCertificate) {
    //            close<InvalidServerCertificate>("server certificate
    //            verification "
    //                                            "failed. OpenSSL error " +
    //                    std::to_string(verifyResult),
    //                ec);
    //        }

    if (ec) {
        close(ec);
        return;
    }

    if (!m_getHandshake) {
        callback(shared_from_this());
        return;
    }

    auto buffer = prepareOutBuffer(m_getHandshake());
    asio::async_write(m_socket, buffer, asio::wrap(m_strand, [
        this,
        self = shared_from_this(),
        callback = std::move(callback)
    ](auto &ec, auto) mutable {
        this->onHandshakeSent(ec, std::move(callback));
    }));
}

void Connection::onHandshakeSent(
    const std::error_code &ec, std::function<void(Ptr)> callback)
{
    if (ec) {
        close(ec);
        return;
    }

    asyncRead(
        [ this, self = shared_from_this(), callback = std::move(callback) ](
            auto &ec) mutable {
            this->onHandshakeReceived(ec, std::move(callback));
        });
}

void Connection::onHandshakeReceived(
    const std::error_code &ec, std::function<void(Ptr)> callback)
{
    if (ec)
        close(ec);
    else if (auto userEc = m_onHandshakeResponse(std::move(m_inData)))
        close(userEc);
    else
        callback(shared_from_this());
}

void Connection::readLoop()
{
    asyncRead([ this, self = shared_from_this() ](const std::error_code &ec) {
        if (ec) {
            close(ec);
            return;
        }

        m_onMessageReceived(std::move(m_inData));
        readLoop();
    });
}

template <typename Handler> void Connection::asyncRead(Handler handler)
{
    asio::async_read(m_socket, headerToBuffer(m_inHeader),
        asio::wrap(m_strand, [ this, handler = std::move(handler) ](
                                 auto &ec, auto) mutable {
            if (ec) {
                handler(ec);
                return;
            }

            const std::size_t size = ntohl(m_inHeader);
            m_inData.resize(size);

            asio::async_read(m_socket, asio::buffer(m_inData),
                asio::wrap(m_strand, [handler = std::move(handler)](auto &ec,
                                         auto) mutable { handler(ec); }));
        }));
}

std::vector<asio::ip::basic_resolver_entry<asio::ip::tcp>>
Connection::shuffleEndpoints(
    asio::ip::basic_resolver_iterator<asio::ip::tcp> iterator)
{
    std::vector<decltype(iterator)::value_type> endpoints;
    std::move(iterator, decltype(iterator){}, std::back_inserter(endpoints));

    std::random_device rd;
    std::default_random_engine engine{rd()};
    std::shuffle(endpoints.begin(), endpoints.end(), engine);

    return endpoints;
}

void Connection::close(const std::error_code &reason)
{
    m_strand.dispatch([&] {
        std::error_code ec;
        m_socket.shutdown(ec);
        m_socket.lowest_layer().shutdown(
            asio::ip::tcp::socket::shutdown_both, ec);
        m_socket.lowest_layer().close(ec);
        m_onClosed(shared_from_this(), reason);
    });
}

asio::mutable_buffers_1 Connection::headerToBuffer(std::uint32_t &header)
{
    return {static_cast<void *>(&header), sizeof(header)};
}

Connection::Ptr createConnection(asio::io_service &ioService,
    asio::ssl::context &context, const bool verifyServerCertificate,
    std::function<std::string()> &getHandshake,
    std::function<std::error_code(std::string)> &onHandshakeResponse,
    std::function<void(std::string)> onMessageReceived,
    std::function<void(Connection::Ptr, const std::error_code &ec)> onClosed)
{
    return std::make_shared<Connection>(ioService, context,
        verifyServerCertificate, getHandshake, onHandshakeResponse,
        std::move(onMessageReceived), std::move(onClosed));
}

} // namespace communication
} // namespace one
