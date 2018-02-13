/**
 * @file macaroon.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "macaroon.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {

Macaroon::Macaroon(std::string macaroon)
    : m_macaroon{std::move(macaroon)}
{
}

std::unique_ptr<ProtocolClientMessage> Macaroon::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    clientMsg->mutable_macaroon()->mutable_macaroon()->swap(m_macaroon);
    return clientMsg;
}

std::string Macaroon::toString() const
{
    std::stringstream stream;
    stream << "type: 'Macaroon', macaroon: '" << m_macaroon << "'";
    return stream.str();
}

} // namespace messages
} // namespace one
