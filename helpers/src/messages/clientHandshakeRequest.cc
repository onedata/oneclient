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
    std::string sessionId, std::string macaroon, std::string version)
    : m_sessionId{std::move(sessionId)}
    , m_macaroon{std::move(macaroon)}
    , m_version{std::move(version)}
{
}

std::string ClientHandshakeRequest::toString() const
{
    std::stringstream stream;
    stream << "type: 'ClientHandshakeRequest', session ID: '" << m_sessionId
           << "', macaroon: ";

    if (m_macaroon)
        stream << "'" << m_macaroon.get() << "'";
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

    if (m_macaroon) {
        auto macaroonMsg = handshakeRequestMsg->mutable_macaroon();
        macaroonMsg->set_macaroon(m_macaroon.get());
    }

    handshakeRequestMsg->set_version(m_version);

    return clientMsg;
}

} // namespace messages
} // namespace one
