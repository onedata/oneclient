/**
* @file messageStreamReset.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef HELPERS_MESSAGES_MESSAGE_STREAM_RESET_H
#define HELPERS_MESSAGES_MESSAGE_STREAM_RESET_H

#include "messages/serverMessage.h"

#include <memory>

namespace one {
namespace messages {

/**
* The MessageStreamReset class represents a message that is sent by the server
* to reset message streams.
*/
class MessageStreamReset : public ServerMessage {
public:
    /**
    * Constructor.
    * @param serverMessage Protocol Buffers message representing @c
    * MessageStreamReset counterpart.
    */
    MessageStreamReset(std::unique_ptr<ProtocolServerMessage> serverMessage);
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_MESSAGE_STREAM_RESET_H
