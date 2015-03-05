/**
* @file serverMessage.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_SERVER_SERVER_MESSAGE_H
#define ONECLIENT_MESSAGES_SERVER_SERVER_MESSAGE_H

namespace one {

// @todo Replace definition with forward declaration after introduction of new
// protocol.
namespace clproto {
class ServerMessage {
};
}

namespace client {

/**
* The ServerMessage class represents a message that can by sent form the server
* to the client.
*/
class ServerMessage {
public:
    using ProtocolServerMessage = one::clproto::ServerMessage;

    virtual ~ServerMessage() = default;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_MESSAGES_SERVER_SERVER_MESSAGE_H