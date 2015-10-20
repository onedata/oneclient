/**
 * @file token.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/token.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {

Token::Token(std::string token)
    : m_token{std::move(token)}
{
}

std::unique_ptr<ProtocolClientMessage> Token::serialize() const
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    clientMsg->mutable_token()->set_value(m_token);
    return clientMsg;
}

std::string Token::toString() const
{
    std::stringstream stream;
    stream << "type: 'Token', token: '" << m_token << "'";
    return stream.str();
}

} // namespace messages
} // namespace one
