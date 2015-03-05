/**
* @file clientMessage.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_CLIENT_CLIENT_MESSAGE_H
#define ONECLIENT_MESSAGES_CLIENT_CLIENT_MESSAGE_H

#include <memory>

namespace one {

// @todo Replace definition with forward declaration after introduction of new
// protocol.
namespace clproto {
class ClientMessage {
};
}

namespace client {

class ClientMessageSerializer;

/**
* The ClientMessage class represents a message that can by sent from the client
* to the server.
*/
class ClientMessage {
public:
    virtual ~ClientMessage() = default;

    /**
    * @return Unique pointer to a @ClientMessageSerializer instance for the @c
    * ClientMessage.
    */
    virtual std::unique_ptr<ClientMessageSerializer>
    createSerializer() const = 0;
};

/**
* The ClientMessageSerializer class is responsible for creation of @c
* ClientMessage counterpart using Protocol Buffers standard.
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
    virtual std::unique_ptr<ProtocolClientMessage>
    serialize(const ClientMessage &clientMessage) const = 0;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_CLIENT_CLIENT_MESSAGE_H