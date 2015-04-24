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

namespace clproto {
class ServerMessage;
}

namespace messages {

using ProtocolServerMessage = one::clproto::ServerMessage;

/**
* The ServerMessage class represents a message that can by sent form the server
* to the client.
*/
class ServerMessage {
public:
    virtual ~ServerMessage() = default;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_SERVER_SERVER_MESSAGE_H
