/**
 * @file connection.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CONNECTION_H
#define HELPERS_COMMUNICATION_CONNECTION_H


#include <exception>
#include <functional>
#include <memory>
#include <string>

namespace one
{
namespace communication
{

/**
 * The Connection class represents a single network-style connection.
 * This class resides on the lowest layer of the communication module.
 */
class Connection
{
public:
    /**
     * Constructor.
     * @param onMessageCallback Callback to be called on received message.
     * @param onFailCallback Callback to be called on connection open failure.
     * @param onOpenCallback Callback to be called on connection open.
     * @param onErrorCallback Callback to be called on open connection's error.
     */
    Connection(
            std::function<void(const std::string&)> onMessageCallback,
            std::function<void(Connection&, std::exception_ptr)> onFailCallback,
            std::function<void(Connection&)> onOpenCallback,
            std::function<void(Connection&)> onErrorCallback);

    /**
     * Destructor.
     * Closes the connections.
     */
    virtual ~Connection();

    /**
     * Sends a message through the connection.
     * @param payload The message to send.
     */
    virtual void send(const std::string &payload) = 0;

protected:
    /**
     * The @p onMessageCallback callback set in @c Connection::Connection(),
     * bound to *this.
     * @param payload Data of the received message.
     */
    std::function<void(const std::string&)> m_onMessageCallback;

    /**
     * The @p onFailCallback callback set in @c Connection::Connection(),
     * bound to *this.
     * @param exception An exception to set as a possible reason for
     * communication failure.
     */
    std::function<void(std::exception_ptr)> m_onFailCallback;

    /**
     * The @p onOpenCallback callback set in @c Connection::Connection(),
     * bound to *this.
     */
    std::function<void()> m_onOpenCallback;

    /**
     * The @p onErrorCallback callback set in @c Connection::Connection(),
     * bound to *this.
     */
    std::function<void()> m_onErrorCallback;
};

} // namespace communication
} // namespace one


#endif // HELPERS_COMMUNICATION_CONNECTION_H
