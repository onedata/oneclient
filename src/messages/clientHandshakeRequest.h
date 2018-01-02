/**
 * @file clientHandshakeRequest.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_HANDSHAKE_REQUEST_H
#define HELPERS_MESSAGES_HANDSHAKE_REQUEST_H

#include "clientMessage.h"

#include <boost/optional.hpp>

#include <memory>
#include <string>

namespace one {
namespace messages {

/**
 * The HandshakeRequest class represents a message that is sent by the client to
 * establish session.
 */
class ClientHandshakeRequest : public ClientMessage {
public:
    /**
     * Constructor.
     * @param sessionId Id of session to be used in handshake
     * */
    ClientHandshakeRequest(std::string sessionId);

    /**
     * Constructor.
     * @param sessionId Id of session to be used in handshake
     * @param token Access token used to established session
     */
    ClientHandshakeRequest(std::string sessionId, std::string token);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_sessionId;
    boost::optional<std::string> m_token;
};

} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_HANDSHAKE_REQUEST_H
