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
    friend std::unique_ptr<ProtocolClientMessage> serialize(
        ClientMessage &&msg);

public:
    virtual ~ClientMessage() = default;

    /**
     * @return @c ClientMessage in string format.
     */
    virtual std::string toString() const = 0;

private:
    /**
     * Creates Protocol Buffers message based on provided @c ClientMessage.
     * The instance of ClientMessage is invalidated after its use.
     * @return Unique pointer to Protocol Buffers @c ClientMessage instance.
     */
    virtual std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() = 0;
};

inline std::unique_ptr<ProtocolClientMessage> serialize(ClientMessage &&msg)
{
    return msg.serializeAndDestroy();
}

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_CLIENT_MESSAGE_H
