/**
 * @file handshakeResponse.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/handshakeResponse.h"

#include "messages.pb.h"

namespace one {
namespace messages {

HandshakeResponse::HandshakeResponse(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto &handshakeResponseMsg = serverMessage->handshake_response();
    m_sessionId = handshakeResponseMsg.session_id();
}

const std::string &HandshakeResponse::sessionId() const { return m_sessionId; }

} // namespace messages
} // namespace one
