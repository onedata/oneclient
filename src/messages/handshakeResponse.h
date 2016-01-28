/**
 * @file handshakeResponse.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_HANDSHAKE_RESPONSE_H
#define HELPERS_MESSAGES_HANDSHAKE_RESPONSE_H

#include "serverMessage.h"

#include "messages.pb.h"

#include <memory>
#include <string>
#include <vector>

namespace one {
namespace messages {

/**
 * The HandshakeResponse class represents a message that is sent by the server
 * to
 * confirm session establishment.
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
     * @return Session id returned by the server.
     */
    const std::string &sessionId() const;

    virtual std::string toString() const override;

private:
    std::string m_sessionId;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_HANDSHAKE_RESPONSE_H
