/**
 * @file readEventSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/readEventSubscription.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

ReadEventSubscription::ReadEventSubscription(
    std::unique_ptr<messages::ProtocolServerMessage> serverMessage)
{
    auto &eventSubscriptionMsg = serverMessage->event_subscription();
    auto &readEventSubscriptionMsg =
        eventSubscriptionMsg.read_event_subscription();
    m_id = readEventSubscriptionMsg.id();
    if (readEventSubscriptionMsg.has_counter_threshold())
        m_counterThreshold = readEventSubscriptionMsg.counter_threshold();
    if (readEventSubscriptionMsg.has_time_threshold())
        m_timeThreshold = std::chrono::milliseconds{
            readEventSubscriptionMsg.time_threshold()};
    if (readEventSubscriptionMsg.has_size_threshold())
        m_sizeThreshold = readEventSubscriptionMsg.size_threshold();
}

ReadEventSubscription::ReadEventSubscription(
    uint64_t id, size_t counterThreshold,
    std::chrono::milliseconds timeThreshold, size_t sizeThreshold)
    : m_id{id}
    , m_counterThreshold{counterThreshold}
    , m_timeThreshold{std::move(timeThreshold)}
    , m_sizeThreshold{sizeThreshold}
{
}

} // namespace events
} // namespace client
} // namespace one
