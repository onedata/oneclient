/**
 * @file connection.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_COMMUNICATION_CONNECTION_H
#define HELPERS_COMMUNICATION_CONNECTION_H

#include "etls/tlsApplication.h"
#include "etls/tlsSocket.h"

namespace one {
namespace communication {

/**
 * @c Connection class represents a single abstract connection between the
 * client and a remote endpoint.
 */
class Connection {
public:
    using Callback = std::function<void(const std::error_code &)>;

    virtual ~Connection() = default;

    /**
     * Sends a message through the managed connection.
     * @param message The message to send.
     * @param callback Callback called when the message is successfuly sent or
     * failed with error.
     */
    virtual void send(std::string message, Callback callback) = 0;

    /**
     * Starts the managed connection.
     */
    virtual void connect() = 0;
};

} // namespace communication
} // namespace one

#endif // HELPERS_COMMUNICATION_CONNECTION_H
