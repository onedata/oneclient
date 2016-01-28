/**
 * @file getConfiguration.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_GET_CONFIGURATION_H
#define ONECLIENT_MESSAGES_GET_CONFIGURATION_H

#include "messages/clientMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The GetConfiguration class represents a message that is sent by the client
 * to get configuration from the server.
 */
class GetConfiguration : public ClientMessage {
public:
    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_GET_CONFIGURATION_H
