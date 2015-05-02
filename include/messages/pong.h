/**
* @file pong.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_PONG_H
#define ONECLIENT_MESSAGES_PONG_H

#include "messages/serverMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
* The Pong class represents a message that is sent by the server to
* confirm session establishment.
*/
class Pong : public ServerMessage {
public:
    /**
    * Constructor.
    * @param serverMessage Protocol Buffers message representing @c
    * Pong counterpart.
    */
    Pong(std::unique_ptr<ProtocolServerMessage> serverMessage);

    virtual std::string toString() const override;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_PONG_H
