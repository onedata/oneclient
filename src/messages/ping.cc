/**
 * @file ping.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/ping.h"

#include "messages.pb.h"

namespace one {
namespace messages {

std::string Ping::toString() const { return "type: 'Ping'"; }

std::unique_ptr<ProtocolClientMessage> Ping::serialize() const
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    clientMsg->mutable_ping();

    return clientMsg;
}

} // namespace messages
} // namespace one
