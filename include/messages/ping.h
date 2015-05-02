/**
* @file ping.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_MESSAGES_PING_H
#define ONECLIENT_MESSAGES_PING_H

#include "messages/clientMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
* The Ping class represents a message that is sent by the client to
* establish session.
*/
class Ping : public ClientMessage {
public:
    virtual std::string toString() const override;

    virtual std::unique_ptr<ProtocolClientMessage> serialize() const override;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_PING_H
