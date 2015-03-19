/**
 * @file writeEventSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/server/writeEventSubscription.h"

namespace one {
namespace client {

WriteEventSubscription::WriteEventSubscription(
    std::unique_ptr<messages::server::ServerMessage::ProtocolServerMessage>
        serverMessage)
{
    // @todo Complete implementation after integration with new protocol,
    // consider moving 'messages' directory do helpers project
}

WriteEventSubscription::WriteEventSubscription(uint64_t id,
    size_t counterThreshold, std::chrono::milliseconds timeThreshold,
    size_t sizeThreshold)
    : m_id{id}
    , m_counterThreshold{counterThreshold}
    , m_timeThreshold{timeThreshold}
    , m_sizeThreshold{sizeThreshold}
{
}

} // namespace client
} // namespace one
