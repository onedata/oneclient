/**
 * @file handshakeRequest.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/handshakeRequest.h"

#include "client_messages.pb.h"

namespace one {
namespace messages {

HandshakeRequest::HandshakeRequest(std::string sessionId)
    : m_sessionId{std::move(sessionId)}
{
}

HandshakeRequest::HandshakeRequest(std::string sessionId, std::string token)
    : m_sessionId{std::move(sessionId)}
    , m_token{std::move(token)}
{
}

std::unique_ptr<ProtocolClientMessage> HandshakeRequest::serialize() const
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto handshakeRequestMsg = clientMsg->mutable_handshake_request();
    auto tokenMsg = handshakeRequestMsg->mutable_token();
    handshakeRequestMsg->set_session_id(m_sessionId);
    if (m_token)
        tokenMsg->set_value(m_token.get());

    return clientMsg;
}

} // namespace messages
} // namespace one
