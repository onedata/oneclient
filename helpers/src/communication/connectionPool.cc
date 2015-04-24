/**
 * @file connectionPool.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication/connectionPool.h"

#include "communication/connection.h"
#include "communication/exception.h"
#include "logging.h"
#include "scheduler.h"

#include <algorithm>
#include <cassert>
#include <exception>
#include <future>

static constexpr std::chrono::seconds WAIT_FOR_CONNECTION{5};

namespace
{
bool eq(const std::unique_ptr<one::communication::Connection> &u,
        const one::communication::Connection &r)
{
    return u.get() == &r;
}
}

namespace one
{
namespace communication
{

ConnectionPool::ConnectionPool(const unsigned int connectionsNumber,
                               std::string uri,
                               std::shared_ptr<Scheduler> scheduler)
    : m_uri{std::move(uri)}
    , m_connectionsNumber{connectionsNumber}
    , m_scheduler{std::move(scheduler)}
{
}

ConnectionPool::~ConnectionPool()
{
    close();
}

void ConnectionPool::close()
{
    std::unique_lock<std::mutex> lock{m_connectionsMutex};

    DLOG(INFO) << "Sending "  << m_goodbyes.size() << " goodbye messages " <<
                  "through " << m_openConnections.size() << " connections.";

    for(const auto &connection: m_openConnections)
        for(const auto &goodbye: m_goodbyes)
            sendHandshakeMessage(*connection, goodbye());

    m_futureConnections.clear();
    m_openConnections.clear();
}

void ConnectionPool::send(const std::string &payload)
{
    std::unique_lock<std::mutex> lock{m_connectionsMutex};
    addConnections();

    m_connectionStatusChanged.wait_for(lock, WAIT_FOR_CONNECTION,
            [&]{ return !m_openConnections.empty() || m_futureConnections.empty(); });

    if(m_openConnections.empty())
    {
        const auto error = takeConnectionError();

        if(error)
            std::rethrow_exception(error);

        throw ConnectionError{"no open connections available"};
    }

    m_openConnections.front()->send(payload);

    m_openConnections.splice(m_openConnections.end(), m_openConnections,
                             m_openConnections.begin());

    takeConnectionError();
}

void ConnectionPool::setOnMessageCallback(std::function<void(const std::string&)> onMessageCallback)
{
    m_onMessageCallback = std::move(onMessageCallback);
}

void ConnectionPool::onFail(Connection &connection, std::exception_ptr exception)
{
    DLOG(WARNING) << "onFail called for connection " << &connection;

    namespace p = std::placeholders;
    std::lock_guard<std::mutex> guard{m_connectionsMutex};
    m_futureConnections.remove_if(std::bind(eq, p::_1, std::cref(connection)));

    if(exception)
    {
        std::lock_guard<std::mutex> guard{m_connectionErrorMutex};
        m_connectionError = exception;
    }

    m_connectionStatusChanged.notify_all();
}

void ConnectionPool::onOpen(Connection &connection)
{
    DLOG(WARNING) << "onOpen called for connection " << &connection;

    namespace p = std::placeholders;
    std::lock_guard<std::mutex> guard{m_connectionsMutex};

    const auto it = std::find_if(m_futureConnections.begin(),
                                 m_futureConnections.end(),
                                 std::bind(eq, p::_1, std::cref(connection)));

    assert(it != m_futureConnections.cend());

    for(const auto &handshake: m_handshakes)
        sendHandshakeMessage(**it, handshake());

    m_openConnections.splice(m_openConnections.begin(), m_futureConnections, it);
    m_connectionStatusChanged.notify_all();
}

void ConnectionPool::onError(Connection &connection)
{
    DLOG(WARNING) << "onError called for connection " << &connection;

    namespace p = std::placeholders;
    std::lock_guard<std::mutex> guard{m_connectionsMutex};
    m_openConnections.remove_if(std::bind(eq, p::_1, std::cref(connection)));

    m_connectionStatusChanged.notify_all();
}

std::exception_ptr ConnectionPool::takeConnectionError()
{
    std::lock_guard<std::mutex> guard{m_connectionErrorMutex};
    const auto error = m_connectionError;
    m_connectionError = std::exception_ptr{};
    return error;
}

void ConnectionPool::sendHandshakeMessage(Connection &conn,
                                          const std::string &payload)
{
    try
    {
        conn.send(payload);
    }
    catch(Exception &e)
    {
        LOG(ERROR) << "Error sending handshake message: " << e.what();
    }
}

std::function<void()> ConnectionPool::addHandshake(std::function<std::string()> handshake,
                                                   std::function<std::string()> goodbye)
{
    std::lock_guard<std::mutex> guard{m_connectionsMutex};

    for(const auto &connection: m_openConnections)
        sendHandshakeMessage(*connection, handshake());

    auto handshakeIt = m_handshakes.emplace(m_handshakes.end(), std::move(handshake));
    auto goodbyeIt = m_goodbyes.emplace(m_goodbyes.begin(), std::move(goodbye));

    return [=]{
        std::lock_guard<std::mutex> guard{m_connectionsMutex};

        for(const auto &connection: m_openConnections)
            sendHandshakeMessage(*connection, (*goodbyeIt)());

        m_handshakes.erase(handshakeIt);
        m_goodbyes.erase(goodbyeIt);
    };
}

void ConnectionPool::recreate()
{
    std::unique_lock<std::mutex> connectionsLock{m_connectionsMutex, std::defer_lock};
    std::unique_lock<std::mutex> closingConnectionsLock{m_closingConnectionsMutex, std::defer_lock};
    std::lock(connectionsLock, closingConnectionsLock);

    m_futureConnections.clear();
    m_closingConnections.splice(m_closingConnections.begin(), m_openConnections);

    closingConnectionsLock.release();
    addConnections();

    m_scheduler->schedule(WAIT_FOR_CONNECTION, [this]{
        std::lock_guard<std::mutex> guard{m_closingConnectionsMutex};
        m_closingConnections.clear();
    });
}

std::function<void()> ConnectionPool::addHandshake(std::function<std::string()> handshake)
{
    std::lock_guard<std::mutex> guard{m_connectionsMutex};

    for(const auto &connection: m_openConnections)
        sendHandshakeMessage(*connection, handshake());

    auto it = m_handshakes.emplace(m_handshakes.end(), std::move(handshake));

    return [=]{
        std::lock_guard<std::mutex> guard{m_connectionsMutex};
        m_handshakes.erase(it);
    };
}

void ConnectionPool::addConnections()
{
    try
    {
        for(auto i = m_futureConnections.size() + m_openConnections.size();
            i < m_connectionsNumber; ++i)
        {
            m_futureConnections.emplace_back(createConnection());
        }
    }
    catch(ConnectionError &e)
    {
        LOG(ERROR) << "Some or all connections couldn't be created: " << e.what();
    }
}

} // namespace communication
} // namespace one
