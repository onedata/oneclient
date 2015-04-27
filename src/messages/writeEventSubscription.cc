/**
 * @file writeEventSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/writeEventSubscription.h"

#include "server_messages.pb.h"

namespace one {
namespace client {
namespace events {

WriteEventSubscription::WriteEventSubscription(
    std::unique_ptr<messages::ProtocolServerMessage> serverMessage)
{
    auto &eventSubscriptionMsg = serverMessage->event_subscription();
    auto &writeEventSubscriptionMsg =
        eventSubscriptionMsg.write_event_subscription();
    m_id = writeEventSubscriptionMsg.id();
    if (writeEventSubscriptionMsg.has_counter_threshold())
        m_counterThreshold = writeEventSubscriptionMsg.counter_threshold();
    if (writeEventSubscriptionMsg.has_time_threshold())
        m_timeThreshold = std::chrono::milliseconds{
            writeEventSubscriptionMsg.time_threshold()};
    if (writeEventSubscriptionMsg.has_size_threshold())
        m_sizeThreshold = writeEventSubscriptionMsg.size_threshold();
}

WriteEventSubscription::WriteEventSubscription(
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
