/**
* @file clientMessage.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef HELPERS_MESSAGES_CLIENT_MESSAGE_H
#define HELPERS_MESSAGES_CLIENT_MESSAGE_H

#include <memory>
#include <string>

namespace one {

namespace clproto {
class ClientMessage;
}

namespace messages {

using ProtocolClientMessage = one::clproto::ClientMessage;

/**
* The ClientMessage class represents a message that can by sent form the client
* to the server.
*/
class ClientMessage {
public:
    virtual ~ClientMessage() = default;

    /**
     * @return @c ClientMessage in string format.
     */
    virtual std::string toString() const = 0;

    /**
    * Creates Protocol Buffers message based on provided @c ClientMessage.
    * @return Unique pointer to Protocol Buffers @c ClientMessage instance.
    */
    virtual std::unique_ptr<ProtocolClientMessage> serialize() const = 0;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_CLIENT_MESSAGE_H
