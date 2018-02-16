/**
 * @file persistentConnection.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "persistentConnection.h"

#include "exception.h"
#include "logging.h"

#include <asio.hpp>
#include <asio/ip/tcp.hpp>
#include <asio/steady_timer.hpp>
#include <openssl/ssl.h>

#include <future>

using namespace std::placeholders;
using namespace std::literals;

namespace one {
namespace communication {

const std::string PersistentConnection::CLPROTO_UPGRADE_ENDPOINT{"/clproto"};
const std::string PersistentConnection::CLPROTO_UPGRADE_RESPONSE_STATUS{
    "HTTP/1.1 101 Switching Protocols"};

PersistentConnection::PersistentConnection(std::string host,
    const unsigned short port, std::shared_ptr<asio::ssl::context> context,
    std::function<void(std::string)> onMessage,
    std::function<void(PersistentConnection &)> onReady,
    std::function<std::string()> getHandshake,
    std::function<std::error_code(std::string)> onHandshakeResponse,
    std::function<void(std::error_code)> onHandshakeDone)
    : m_host{std::move(host)}
    , m_port{port}
    , m_context{std::move(context)}
    , m_onMessage{std::move(onMessage)}
    , m_onReady{std::move(onReady)}
    , m_getHandshake{std::move(getHandshake)}
    , m_onHandshakeResponse{std::move(onHandshakeResponse)}
    , m_callback{std::move(onHandshakeDone)}
{
    LOG_FCALL() << LOG_FARG(host) << LOG_FARG(port);
}

PersistentConnection::~PersistentConnection()
{
    LOG_FCALL();

    close();
}

void PersistentConnection::connect()
{
    LOG_FCALL();

    LOG_DBG(2) << "Connecting to " << m_host << ":" << m_port;

    m_socket = std::make_shared<etls::TLSSocket>(m_app, m_context);
    m_socket->connectAsync(m_socket, m_host, m_port,
        createCallback<etls::TLSSocket::Ptr>([=](auto) { this->onConnect(); }));
}

void PersistentConnection::onConnect()
{
    LOG_FCALL();

    LOG_DBG(2) << "Connected to " << m_host << ":" << m_port;

    upgrade();
}

void PersistentConnection::upgrade()
{
    LOG_FCALL();

    std::stringstream requestStream;
    requestStream << "GET " << CLPROTO_UPGRADE_ENDPOINT << " HTTP/1.1\r\n";
    requestStream << "Host: " << m_host << "\r\n";
    requestStream << "Connection: upgrade\r\n";
    requestStream << "Upgrade: clproto\r\n\r\n";

    LOG_DBG(2) << "Sending socket upgrade request: \n" << requestStream.str();

    m_socket->sendAsync(m_socket, prepareRawOutBuffer(requestStream.str()),
        createCallback([=] { this->onUpgradeRequestSent(); }));
}

void PersistentConnection::onUpgradeRequestSent()
{
    LOG_FCALL();

    LOG_DBG(2) << "Upgrade request sent to " << m_host << ":" << m_port;

    // Read the raw stream until end of HTTP response
    asyncReadRawUntil(
        "\r\n\r\n", [=](auto) { this->onUpgradeResponseReceived(); });
}

void PersistentConnection::onUpgradeResponseReceived()
{
    LOG_FCALL();

    LOG_DBG(2) << "Upgrade response received: " << m_inData << "\n";

    std::string httpResponseStatus;
    std::istringstream iss(std::move(m_inData));
    std::getline(iss, httpResponseStatus);
    if (!boost::starts_with(
            httpResponseStatus, CLPROTO_UPGRADE_RESPONSE_STATUS)) {
        LOG(ERROR) << "Invalid response during protocol upgrade: "
                   << httpResponseStatus << ". Expected:\n '"
                   << CLPROTO_UPGRADE_RESPONSE_STATUS << "'";
        std::error_code ec;
        onError(ec);
    }
    else {
        LOG_DBG(2) << "Socket protocol successfully upgraded to clproto";

        m_inData.clear();

        if (!m_getHandshake) {
            LOG_DBG(2) << "Clproto handshake not required during connection - "
                          "skipping";
            start();
            return;
        }

        auto buffer = prepareOutBuffer(m_getHandshake());
        m_socket->sendAsync(
            m_socket, buffer, createCallback([=] { onHandshakeSent(); }));
    }
}

void PersistentConnection::onHandshakeSent()
{
    LOG_FCALL();

    LOG_DBG(2) << "Handshake sent - waiting for response...";

    asyncRead([=](auto) { this->onHandshakeReceived(); });
}

void PersistentConnection::onHandshakeReceived()
{
    LOG_FCALL();

    LOG_DBG(2) << "Handshake received " << m_inData;

    std::error_code ec;
    if (m_onHandshakeResponse)
        ec = m_onHandshakeResponse(std::move(m_inData));

    if (ec)
        onError(ec);
    else
        start();
}

void PersistentConnection::onSent()
{
    LOG_FCALL();

    notify();
    m_onReady(*this);
}

void PersistentConnection::onError(const std::error_code &ec1)
{
    LOG_FCALL();

    LOG(ERROR) << "Error during connection: " << ec1 << "\n";

    m_recreateTimer.expires_at(
        std::chrono::steady_clock::now() + RECREATE_DELAY);

    m_recreateTimer.async_wait([this](auto ec2) {
        if (!ec2)
            this->connect();
    });

    notify(ec1);
    close();
}

void PersistentConnection::send(std::string message, Callback callback)
{
    LOG_FCALL() << LOG_FARG(message.size());

    asio::post(m_app.ioService(), [
        =, message = std::move(message), callback = std::move(callback)
    ]() mutable {
        auto socket = getSocket();
        if (!m_connected || !socket) {
            LOG(ERROR) << "Cannot send message - socket not connected.";
            callback(asio::error::not_connected);
            return;
        }

        m_callback = std::move(callback);
        auto buffer = prepareOutBuffer(std::move(message));
        socket->sendAsync(socket, buffer, createCallback([=] { onSent(); }));
    });
}

std::array<asio::const_buffer, 2> PersistentConnection::prepareOutBuffer(
    std::string message)
{
    LOG_FCALL() << LOG_FARG(message.size());

    m_outHeader = htonl(message.size());
    m_outData = std::move(message);
    return {{headerToBuffer(m_outHeader), asio::buffer(m_outData)}};
}

std::array<asio::const_buffer, 1> PersistentConnection::prepareRawOutBuffer(
    std::string message)
{
    LOG_FCALL() << LOG_FARG(message.size());

    m_outData = std::move(message);
    return {{asio::buffer(m_outData)}};
}

void PersistentConnection::readLoop()
{
    LOG_FCALL();

    asyncRead([=](asio::mutable_buffer) {
        m_onMessage(std::move(m_inData));
        readLoop();
    });
}

void PersistentConnection::close()
{
    LOG_FCALL();

    LOG_DBG(2) << "Closing persistent connection: " << m_connectionId;

    if (!m_socket)
        return;

    ++m_connectionId;
    m_connected = false;

    auto socket = std::atomic_exchange(&m_socket, {});
    socket->closeAsync(socket, {[=] {}, [=](auto) {}});
}

void PersistentConnection::notify(const std::error_code &ec)
{
    LOG_FCALL() << LOG_FARG(ec);

    if (m_callback) {
        decltype(m_callback) callback;
        std::swap(callback, m_callback);
        callback(ec);
    }

    if (!m_connected && m_onHandshakeDone) {
        m_onHandshakeDone(ec);
    }
}

void PersistentConnection::start()
{
    LOG_FCALL();

    notify();
    m_connected = true;
    readLoop();
    m_onReady(*this);
}

etls::TLSSocket::Ptr PersistentConnection::getSocket()
{
    return std::atomic_load(&m_socket);
}

template <typename... Args, typename SF>
etls::Callback<Args...> PersistentConnection::createCallback(SF &&onSuccess)
{
    const int connectionId = m_connectionId;

    auto wrappedSuccess =
        [ =, onSuccess = std::forward<SF>(onSuccess) ](Args && ... args) mutable
    {
        if (m_connectionId == connectionId)
            onSuccess(std::forward<Args>(args)...);
    };

    auto wrappedError = [=](const std::error_code &ec) {
        if (m_connectionId == connectionId)
            onError(ec);
    };

    return {std::move(wrappedSuccess), std::move(wrappedError)};
}

template <typename SF> void PersistentConnection::asyncRead(SF &&onSuccess)
{
    auto onHeaderSuccess = [ =, onSuccess = std::forward<SF>(onSuccess) ](
        asio::mutable_buffer) mutable
    {
        const std::size_t size = ntohl(m_inHeader);
        m_inData.resize(size);

        if (auto socket = getSocket())
            socket->recvAsync(socket, asio::buffer(m_inData),
                createCallback<asio::mutable_buffer>(std::move(onSuccess)));
    };

    if (auto socket = getSocket())
        socket->recvAsync(socket, headerToBuffer(m_inHeader),
            createCallback<asio::mutable_buffer>(std::move(onHeaderSuccess)));
}

template <typename SF>
void PersistentConnection::asyncReadRawUntil(
    std::string delimiter, SF &&onSuccess)
{
    if (auto socket = getSocket()) {
        m_inData.resize(256);
        socket->recvUntilAsyncRaw(socket, asio::buffer(m_inData), delimiter,
            createCallback<asio::mutable_buffer>(std::move(onSuccess)));
    }
}

asio::mutable_buffers_1 PersistentConnection::headerToBuffer(
    std::uint32_t &header)
{
    return {static_cast<void *>(&header), sizeof(header)};
}

std::unique_ptr<Connection> createConnection(std::string host,
    const unsigned short port, std::shared_ptr<asio::ssl::context> context,
    std::function<void(std::string)> onMessage,
    std::function<void(Connection &)> onReady,
    std::function<std::string()> getHandshake,
    std::function<std::error_code(std::string)> onHandshakeResponse,
    std::function<void(std::error_code)> onHandshakeDone)
{
    return std::make_unique<PersistentConnection>(std::move(host), port,
        std::move(context), std::move(onMessage), std::move(onReady),
        std::move(getHandshake), std::move(onHandshakeResponse),
        std::move(onHandshakeDone));
}

} // namespace communication
} // namespace one
