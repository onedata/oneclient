/**
 * @file connectionPool.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CONNECTION_POOL_H
#define HELPERS_COMMUNICATION_CONNECTION_POOL_H


#include <condition_variable>
#include <exception>
#include <functional>
#include <list>
#include <memory>
#include <mutex>
#include <string>

namespace one
{

class Scheduler;

namespace communication
{

class Connection;
class ConnectionError;

/**
 * The ConnectionPool class is responsible for managing and maintaining
 * connections.
 * It provides an interface over the whole pool.
 * @note All of the callback methods (including onMessageCallback set by
 * @c setOnMessageCallback() ) need to be called by a different thread of
 * execution than the thread that manages ConnectionPool instance.
 */
class ConnectionPool
{
public:
    /**
     * Constructor.
     * @param connectionsNumber Number of connections that should be maintained
     * by this pool.
     * @param uri Server's URI to connect to.
     */
    ConnectionPool(const unsigned int connectionsNumber,
                   std::string uri,
                   std::shared_ptr<Scheduler> scheduler);

    /**
     * Destructor.
     * Sends goodbyes on all connections and closes them.
     */
    virtual ~ConnectionPool();

    /**
     * Sends a message through a connection.
     * @param payload The message to be sent.
     */
    virtual void send(const std::string &payload);

    /**
     * Sets a callback that should be called on received message.
     * @param onMessageCallback The callback to be called.
     */
    virtual void setOnMessageCallback(
            std::function<void(const std::string&)> onMessageCallback);

    /**
     * Adds a handshake message to be sent once through each connection open
     * currently and opened in the future.
     * @param handshake A function returning the handshake message to be sent.
     * The message returned by the function will not be cached.
     * @return A function to cancel the handshake.
     */
    virtual std::function<void()> addHandshake(
            std::function<std::string()> handshake);

    /**
     * Adds a handhshake and goodbye message to be sent once through each
     * connection open currently and opened in the future.
     * The goodbye message will be sent on closing the connections or on
     * cancellation of the handshake.
     * @param handshake A function returning the handshake message to be sent.
     * The message returned by the function will not be cached.
     * @param goodbye A function returning the goodbye message to be sent.
     * The message returned by the function will not be cached.
     * @return A function to cancel the handshake.
     */
    virtual std::function<void()> addHandshake(
            std::function<std::string()> handshake,
            std::function<std::string()> goodbye);

    /**
     * Recreates all connections, dropping the old ones after a short grace
     * period.
     */
    void recreate();

protected:
    /**
     * Sends goodbyes and closes all connections.
     * This method is called by destructor. It's reentrant, so it can be called
     * by derived classes to modify destruction order.
     */
    void close();

    /**
     * Calls @c createConnection() as many times as needed in attempt to
     * maintain a set number of connections.
     * The number of connections to maintain is set in the constructor.
     */
    void addConnections();

    /**
     * A method to be called by connections that failed to open.
     * @param connection The connection that failed to open.
     * @param exception An exception to set as a possible reason for
     * communication failure.
     */
    void onFail(Connection &connection, std::exception_ptr exception);

    /**
     * A method to be called by connections that opened succesfuly.
     * @param connection The connection that opened succesfuly.
     */
    void onOpen(Connection &connection);

    /**
     * A method to be called by connections that failed after opening
     * succesfuly.
     * @param connection The connection that failed.
     */
    void onError(Connection &connection);

    /**
     * Creates a new Connection instance to be managed by the pool.
     * @return The new Connection instance.
     */
    virtual std::unique_ptr<Connection> createConnection() = 0;

    /**
     * The onMessageCallback set in @c setOnMessageCallback() .
     * @param message Contents of a received message.
     */
    std::function<void(const std::string&)> m_onMessageCallback =
            [](const std::string&){};

    /**
     * The uri set in @c ConnectionPool::ConnectionPool() .
     */
    const std::string m_uri;

    /**
     * A mutex guarding @c m_connectionError .
     */
    std::mutex m_connectionErrorMutex;

    /**
     * A possibly null pointer to last error encountered while opening a
     * connection.
     */
    std::exception_ptr m_connectionError;

private:
    std::exception_ptr takeConnectionError();
    void sendHandshakeMessage(Connection &conn, const std::string &payload);

    const unsigned int m_connectionsNumber;
    std::mutex m_connectionsMutex;
    std::mutex m_closingConnectionsMutex;
    std::condition_variable m_connectionStatusChanged;
    std::list<std::unique_ptr<Connection>> m_futureConnections;
    std::list<std::unique_ptr<Connection>> m_openConnections;
    std::list<std::unique_ptr<Connection>> m_closingConnections;
    std::list<std::function<std::string()>> m_handshakes;
    std::list<std::function<std::string()>> m_goodbyes;
    std::shared_ptr<Scheduler> m_scheduler;
};

} // namespace communication
} // namespace one


#endif // HELPERS_COMMUNICATION_CONNECTION_POOL_H
