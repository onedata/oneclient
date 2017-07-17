/**
 * @file quotaExceededSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_QUOTA_EXCEEDED_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_QUOTA_EXCEEDED_SUBSCRIPTION_H

#include "event.h"
#include "remoteSubscription.h"

namespace one {
namespace client {
namespace events {

class QuotaExceeded;

/**
 * @c QuotaExceededSubscription represents a subscription for a quota exceeded
 * events that occure in the system.
 */
class QuotaExceededSubscription : public RemoteSubscription {
public:
    /**
     * Constructor.
     * @param handler A callback function that should be executed when quota
     * exceeded events occure.
     */
    QuotaExceededSubscription(EventHandler<QuotaExceeded> handler);

    StreamKey streamKey() const override;

    /**
     * Creates a stream that handles each event separately without aggregation.
     * @see Subscription::createHandle(std::int64_t subscriptionId, Streams
     * &streams, SequencerStream &stream)
     */
    StreamPtr createStream(Manager &manager, SequencerManager &seqManager,
        Scheduler &scheduler) const override;

    std::string toString() const override;

    ProtoSubscriptionPtr serialize() const override;

private:
    EventHandler<QuotaExceeded> m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_QUOTA_EXCEEDED_SUBSCRIPTION_H
