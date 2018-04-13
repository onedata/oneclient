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
    const unsigned short port, asio::io_service &ioService,
    std::shared_ptr<asio::ssl::context> context,
    std::function<void(std::string)> onMessage,
    std::function<void(PersistentConnection &)> onReady,
    std::function<std::string()> getHandshake,
    std::function<std::error_code(std::string)> onHandshakeResponse,
    std::function<void(std::error_code)> onHandshakeDone, bool clProtoUpgrade)
    : m_host{std::move(host)}
    , m_port{port}
    , m_ioService(ioService)
    , m_context{std::move(context)}
    , m_onMessage{std::move(onMessage)}
    , m_onReady{std::move(onReady)}
    , m_getHandshake{std::move(getHandshake)}
    , m_onHandshakeResponse{std::move(onHandshakeResponse)}
    , m_onHandshakeDone{std::move(onHandshakeDone)}
    , m_recreateBackoffDelay{RECREATE_DELAY_INITIAL}
    , m_clProtoUpgrade{clProtoUpgrade}
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

    m_socket = std::make_shared<etls::TLSSocket>(m_ioService, m_context);
    m_socket->connectAsync(m_socket, m_host, m_port,
        createCallback<etls::TLSSocket::Ptr>([self = shared_from_this()](
            auto) { self->onConnect(); }));
}

bool PersistentConnection::connected() const { return m_connected.load(); }

void PersistentConnection::onConnect()
{
    LOG_FCALL();

    LOG_DBG(2) << "Connected to " << m_host << ":" << m_port;

    m_recreateBackoffDelay = RECREATE_DELAY_INITIAL;

    if (m_clProtoUpgrade)
        upgrade();
    else
        start();
}

void PersistentConnection::upgrade()
{
    LOG_FCALL();

    std::stringstream requestStream;
    requestStream << "GET " << CLPROTO_UPGRADE_ENDPOINT << " HTTP/1.1\r\n";
    requestStream << "Host: " << m_host << "\r\n";
    requestStream << "Connection: upgrade\r\n";
    requestStream << "Upgrade: clproto\r\n\r\n";

    LOG_DBG(2) << "Sending socket upgrade request";

    auto buffer = prepareRawOutBuffer(requestStream.str());
    m_socket->sendAsync(m_socket, buffer->asioBufferSequence(),
        createCallback([ self = shared_from_this(), buffer ]() mutable {
            self->onUpgradeRequestSent();
            buffer.reset();
        }));
}

void PersistentConnection::onUpgradeRequestSent()
{
    LOG_FCALL();

    LOG_DBG(2) << "Upgrade request sent to " << m_host << ":" << m_port;

    // Read the raw stream until end of HTTP response
    asyncReadRawUntil("\r\n\r\n", [self = shared_from_this()](auto) {
        self->onUpgradeResponseReceived();
    });
}

void PersistentConnection::onUpgradeResponseReceived()
{
    LOG_FCALL();

    LOG_DBG(2) << "Upgrade response received";

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

        m_socket->sendAsync(m_socket, buffer->asioBufferSequence(),
            createCallback([ self = shared_from_this(), buffer ]() mutable {
                self->onHandshakeSent();
                buffer.reset();
            }));
    }
}

void PersistentConnection::onHandshakeSent()
{
    LOG_FCALL();

    LOG_DBG(2) << "Handshake sent - waiting for response...";

    asyncRead([self = shared_from_this()](
        auto) { self->onHandshakeReceived(); });
}

void PersistentConnection::onHandshakeReceived()
{
    LOG_FCALL();

    LOG_DBG(2) << "Handshake received";

    std::error_code ec;
    if (m_onHandshakeResponse)
        ec = m_onHandshakeResponse(std::move(m_inData));

    if (ec)
        onError(ec);
    else
        start();
}

void PersistentConnection::onSent(Callback &&callback)
{
    LOG_FCALL();

    notify(std::move(callback));
    m_onReady(*this);
}

void PersistentConnection::onError(const std::error_code &ec1)
{
    LOG_FCALL();

    LOG(ERROR) << "Error during connection: [" << ec1 << "] " << ec1.message();

    close();

    auto recreateDelay = nextReconnectionDelay();

    LOG(INFO) << "Reconnecting in " << recreateDelay.count() << " [ms]";

    m_recreateTimer.expires_at(
        std::chrono::steady_clock::now() + recreateDelay);

    m_recreateTimer.async_wait([self = shared_from_this()](auto ec2) {
        if (!ec2)
            self->connect();
    });

    notify({}, ec1);
}

void PersistentConnection::send(std::string message, Callback callback)
{
    LOG_FCALL() << LOG_FARG(message.size());

    LOG_DBG(4) << "Sending binary message: " << LOG_ERL_BIN(message);

    auto socket = getSocket();
    if (!m_connected || !socket) {
        LOG(ERROR) << "Cannot send message - socket not connected.";
        callback(asio::error::not_connected);
        return;
    }

    auto buffer = prepareOutBuffer(std::move(message));

    socket->sendAsync(socket, buffer->asioBufferSequence(), createCallback([
        self = shared_from_this(), buffer, callback = std::move(callback)
    ]() mutable {
        self->onSent(std::move(callback));
        buffer.reset();
    }));
}

std::shared_ptr<SharedConstBufferSequence<1>>
PersistentConnection::prepareOutBuffer(std::string message)
{
    LOG_FCALL() << LOG_FARG(message.size());

    int32_t outHeader = htonl(message.size());
    auto outData =
        std::string(reinterpret_cast<char *>(&outHeader), 4) + message;

    return std::make_shared<SharedConstBufferSequence<1>>(std::move(outData));
}

std::shared_ptr<SharedConstBufferSequence<1>>
PersistentConnection::prepareRawOutBuffer(std::string message)
{
    LOG_FCALL() << LOG_FARG(message.size());

    return std::make_shared<SharedConstBufferSequence<1>>(std::move(message));
}

void PersistentConnection::readLoop()
{
    LOG_FCALL();

    asyncRead([=](asio::mutable_buffer) {
        LOG_DBG(4) << "Received binary message: " << LOG_ERL_BIN(m_inData);

        m_onMessage(std::move(m_inData));
        m_inData.clear();
        readLoop();
    });
}

void PersistentConnection::close()
{
    LOG_FCALL();

    LOG_DBG(2) << "Closing persistent connection: " << m_connectionId;

    m_connected = false;
    ++m_connectionId;

    if (!m_socket)
        return;

    auto socket = std::atomic_exchange(&m_socket, {});
    socket->closeAsync(socket, {[=] {}, [=](auto) {}});
}

void PersistentConnection::notify(
    Callback &&callback, const std::error_code &ec)
{
    LOG_FCALL() << LOG_FARG(ec);

    if (callback) {
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

int PersistentConnection::connectionId() const { return m_connectionId.load(); }

std::chrono::milliseconds PersistentConnection::nextReconnectionDelay()
{
    LOG_FCALL();

    m_recreateBackoffDelay = std::chrono::milliseconds(std::min<unsigned long>(
        m_recreateBackoffDelay.count() * RECREATE_DELAY_FACTOR,
        RECREATE_DELAY_MAX.count()));

    return m_recreateBackoffDelay;
}

etls::TLSSocket::Ptr PersistentConnection::getSocket()
{
    return std::atomic_load(&m_socket);
}

template <typename... Args, typename SF>
etls::Callback<Args...> PersistentConnection::createCallback(SF &&onSuccess)
{
    const int connectionId = m_connectionId;

    auto wrappedSuccess = [
        self = shared_from_this(), connectionId,
        onSuccess = std::forward<SF>(onSuccess)
    ](Args && ... args) mutable
    {
        if (self->connectionId() == connectionId)
            onSuccess(std::forward<Args>(args)...);
    };

    auto wrappedError =
        [ self = shared_from_this(), connectionId ](const std::error_code &ec)
    {
        if (self->connectionId() == connectionId)
            self->onError(ec);
    };

    return {std::move(wrappedSuccess), std::move(wrappedError)};
}

template <typename SF> void PersistentConnection::asyncRead(SF &&onSuccess)
{
    auto onHeaderSuccess =
        [ self = shared_from_this(), onSuccess = std::forward<SF>(onSuccess) ](
            asio::mutable_buffer) mutable
    {
        const std::size_t size = ntohl(self->m_inHeader);
        self->m_inData.clear();
        self->m_inData.resize(size);

        if (auto socket = self->getSocket())
            socket->recvAsync(socket, asio::buffer(self->m_inData),
                self->createCallback<asio::mutable_buffer>(
                    std::move(onSuccess)));
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
        m_inData.clear();
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

std::shared_ptr<Connection> createConnection(std::string host,
    const unsigned short port, asio::io_service &ioService,
    std::shared_ptr<asio::ssl::context> context,
    std::function<void(std::string)> onMessage,
    std::function<void(Connection &)> onReady,
    std::function<std::string()> getHandshake,
    std::function<std::error_code(std::string)> onHandshakeResponse,
    std::function<void(std::error_code)> onHandshakeDone)
{
    return std::make_shared<PersistentConnection>(std::move(host), port,
        ioService, std::move(context), std::move(onMessage), std::move(onReady),
        std::move(getHandshake), std::move(onHandshakeResponse),
        std::move(onHandshakeDone));
}

} // namespace communication
} // namespace one
