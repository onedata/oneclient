/**
* @file readEventSubscription.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "messages/server/readEventSubscription.h"

namespace one {
namespace client {

ReadEventSubscription::ReadEventSubscription(
    std::unique_ptr<ServerMessage::ProtocolServerMessage> serverMessage)
{
    // @todo Complete implementation after integration with new protocol,
    // consider moving 'messages' directory do helpers project
}

ReadEventSubscription::ReadEventSubscription(
    uint64_t id, size_t counterThreshold,
    std::chrono::milliseconds timeThreshold, size_t sizeThreshold)
    : m_id{id}
    , m_counterThreshold{counterThreshold}
    , m_timeThreshold{timeThreshold}
    , m_sizeThreshold{sizeThreshold}
{
}

}; // namespace client
} // namespace one
