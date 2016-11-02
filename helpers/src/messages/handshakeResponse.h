/**
 * @file handshakeResponse.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_HANDSHAKE_RESPONSE_H
#define HELPERS_MESSAGES_HANDSHAKE_RESPONSE_H

#include "errors/handshakeErrors.h"
#include "serverMessage.h"

#include "messages.pb.h"

#include <memory>
#include <string>
#include <system_error>

namespace one {
namespace messages {

/**
 * The HandshakeResponse class represents a message that is sent by the server
 * to confirm session establishment.
 */
class HandshakeResponse : public ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing @c
     * HandshakeResponse counterpart.
     */
    HandshakeResponse(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return true if a token error occurred during handshake, otherwise false
     */
    bool isTokenError() const;

    /**
     * @return handshake status
     */
    std::error_code status() const;

    virtual std::string toString() const override;

private:
    errors::handshake::ErrorCode translateStatus(
        const one::clproto::HandshakeResponse &msg);

    errors::handshake::ErrorCode m_status;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_HANDSHAKE_RESPONSE_H
