/**
 * @file token.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_TOKEN_H
#define ONECLIENT_MESSAGES_TOKEN_H

#include "messages/clientMessage.h"

namespace one {
namespace messages {

/**
 * The @c Token class represents a new token for connection.
 */
class Token : public ClientMessage {
public:
    /**
     * Constructor.
     * @param token The token to send.
     */
    Token(std::string token);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_token;
};

} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_TOKEN_H
