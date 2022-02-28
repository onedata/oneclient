/**
 * @file closeSession.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "closeSession.h"

#include "messages.pb.h"


namespace one {
namespace messages {

std::string CloseSession::toString() const
{
    return "type: 'CloseSession'";
}

std::unique_ptr<ProtocolClientMessage> CloseSession::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    clientMsg->mutable_close_session();

    return clientMsg;
}

} // namespace messages
} // namespace one
