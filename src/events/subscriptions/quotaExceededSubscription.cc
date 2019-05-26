/**
 * @file quotaExceededSubscription.cc
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

QuotaExceededSubscription::QuotaExceededSubscription(
    EventHandler<QuotaExceeded> handler)
    : m_handler{std::move(handler)}
{
}

StreamKey QuotaExceededSubscription::streamKey() const
{
    return StreamKey::QUOTA_EXCEEDED;
}

StreamPtr QuotaExceededSubscription::createStream(Manager & /*manager*/,
    SequencerManager & /*seqManager*/, Scheduler & /*scheduler*/) const
{
    auto aggregator = std::make_unique<KeyAggregator<QuotaExceeded>>();
    auto emitter = std::make_unique<CounterEmitter<QuotaExceeded>>(1);
    auto handler = std::make_unique<LocalHandler<QuotaExceeded>>(m_handler);

    return std::make_unique<AsyncStream>(
        std::make_unique<TypedStream<QuotaExceeded>>(
            std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string QuotaExceededSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'QuotaExceeded'";
    return stream.str();
}

ProtoSubscriptionPtr QuotaExceededSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    subscriptionMsg->mutable_quota_exceeded();

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
