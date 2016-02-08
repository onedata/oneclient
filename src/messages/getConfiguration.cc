/**
 * @file getConfiguration.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getConfiguration.h"

#include "messages.pb.h"

namespace one {
namespace messages {

std::string GetConfiguration::toString() const
{
    return "type: 'GetConfiguration'";
}

std::unique_ptr<ProtocolClientMessage> GetConfiguration::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    clientMsg->mutable_get_configuration();

    return clientMsg;
}

} // namespace messages
} // namespace one
