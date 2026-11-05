/**
 * @file closeSession.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_CLOSE_SESSION_H
#define ONECLIENT_MESSAGES_CLOSE_SESSION_H

#include "messages/clientMessage.h"

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The CloseSession class represents a message that is sent by the client
 * to inform the Oneprovider of the intent to close the current connection.
 */
class CloseSession : public ClientMessage {
public:
    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_CLOSE_SESSION_H
