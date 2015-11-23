/**
 * @file getProtocolVersion.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/getProtocolVersion.h"

#include "messages.pb.h"

namespace one {
namespace messages {

std::string GetProtocolVersion::toString() const
{
    return "type: 'GetProtocolVersion'";
}

std::unique_ptr<ProtocolClientMessage> GetProtocolVersion::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    clientMsg->mutable_get_protocol_version();

    return clientMsg;
}

} // namespace messages
} // namespace one
