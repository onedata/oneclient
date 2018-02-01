/**
 * @file clientHandshakeRequest.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/clientHandshakeRequest.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {

ClientHandshakeRequest::ClientHandshakeRequest(std::string sessionId)
    : m_sessionId{std::move(sessionId)}
{
}

ClientHandshakeRequest::ClientHandshakeRequest(
    std::string sessionId, std::string token, std::string version)
    : m_sessionId{std::move(sessionId)}
    , m_token{std::move(token)}
    , m_version{std::move(version)}
{
}

std::string ClientHandshakeRequest::toString() const
{
    std::stringstream stream;
    stream << "type: 'ClientHandshakeRequest', session ID: '" << m_sessionId
           << "', token: ";

    if (m_token)
        stream << "'" << m_token.get() << "'";
    else
        stream << "'undefined'";

    stream << ", version: '" << m_version << "'";

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage>
ClientHandshakeRequest::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto handshakeRequestMsg = clientMsg->mutable_client_handshake_request();
    handshakeRequestMsg->set_session_id(m_sessionId);

    if (m_token) {
        auto tokenMsg = handshakeRequestMsg->mutable_token();
        tokenMsg->set_value(m_token.get());
    }

    handshakeRequestMsg->set_version(m_version);

    return clientMsg;
}

} // namespace messages
} // namespace one
