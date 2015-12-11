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

Ping::Ping(std::string data)
    : m_data{data}
{
}

std::string Ping::toString() const { return "type: 'Ping'"; }

std::unique_ptr<ProtocolClientMessage> Ping::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<ProtocolClientMessage>();
    auto ping = clientMsg->mutable_ping();
    if (m_data)
        ping->set_data(m_data.get());

    return clientMsg;
}

} // namespace messages
} // namespace one
