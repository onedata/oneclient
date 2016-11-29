/**
 * @file quotaSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/events.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

QuotaSubscription::QuotaSubscription(EventHandler handler)
    : m_handler{std::move(handler)}
{
}

const std::string &QuotaSubscription::routingKey() const
{
    return m_routingKey;
}

StreamPtr QuotaSubscription::createStream(std::int64_t streamId,
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator>();
    auto emitter = std::make_unique<CounterEmitter>(1);
    auto handler = std::make_unique<LocalHandler>(std::move(m_handler));

    return std::make_unique<AsyncStream>(std::make_unique<LocalStream>(
        std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string QuotaSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'Quota'";
    return stream.str();
}

ProtoSubscriptionPtr QuotaSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    subscriptionMsg->mutable_quota_subscription();

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
