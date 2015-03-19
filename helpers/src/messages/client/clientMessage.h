/**
* @file clientMessage.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef HELPERS_MESSAGES_CLIENT_CLIENT_MESSAGE_H
#define HELPERS_MESSAGES_CLIENT_CLIENT_MESSAGE_H

#include <memory>

namespace one {

namespace clproto {
class ClientMessage;
}

namespace messages {
namespace client {

class ClientMessageSerializer;

/**
* The ClientMessage class represents a message that can by sent form the client
* to the server.
*/
class ClientMessage {
public:
    virtual ~ClientMessage() = default;

    /**
    * Returns a @c ClientMessageSerializer instance for the @c ClientMessage.
    * @return Unique pointer to a @ClientMessageSerializer instance.
    */
    virtual std::unique_ptr<ClientMessageSerializer>
    createSerializer() const = 0;
};

/**
* The ClientMessageSerializer class is responsible for creation of @c
* ClientMessage
* counterpart using Protocol Buffers standard.
*/
class ClientMessageSerializer {
public:
    using ProtocolClientMessage = one::clproto::ClientMessage;

    virtual ~ClientMessageSerializer() = default;

    /**
    * Creates Protocol Buffers message based on provided @c ClientMessage.
    * @param clientMessage Message to be translated to Protocol Buffers
    * counterpart.
    * @return Unique pointer to Protocol Buffers @c ClientMessage instance.
    */
    virtual std::unique_ptr<ProtocolClientMessage> serialize(
        const ClientMessage &clientMessage) const = 0;
};

} // namespace client
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_CLIENT_CLIENT_MESSAGE_H
