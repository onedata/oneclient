/**
* @file getProtocolVersion.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef HELPERS_MESSAGES_GET_PROTOCOL_VERSION_H
#define HELPERS_MESSAGES_GET_PROTOCOL_VERSION_H

#include "messages/clientMessage.h"

#include <memory>

namespace one {
namespace messages {

/**
* The GetProtocolVersion class represents a message that is sent by the client
* to get communication protocol version used by the server.
*/
class GetProtocolVersion : public ClientMessage {
public:
    virtual std::unique_ptr<ProtocolClientMessage> serialize() const override;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_GET_PROTOCOL_VERSION_H
