/**
 * @file handshakeResponse.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/handshakeResponse.h"

#include <sstream>

namespace one {
namespace messages {

HandshakeResponse::HandshakeResponse(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto &handshakeResponseMsg = serverMessage->handshake_response();
    m_sessionId = handshakeResponseMsg.session_id();
    for (const auto &subscription : handshakeResponseMsg.subscriptions())
        m_subscriptions.emplace_back(subscription);
}

const std::string &HandshakeResponse::sessionId() const { return m_sessionId; }

void HandshakeResponse::moveSubscriptions(
    std::vector<clproto::Subscription> &subscriptions)
{
    subscriptions.swap(m_subscriptions);
}

std::string HandshakeResponse::toString() const
{
    std::stringstream stream;
    stream << "type: 'HandshakeResponse', session ID: '" << m_sessionId << "'";
    return stream.str();
}

} // namespace messages
} // namespace one
