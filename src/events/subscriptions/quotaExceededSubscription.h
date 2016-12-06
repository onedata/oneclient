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

class QuotaExceededSubscription : public RemoteSubscription {
public:
    QuotaExceededSubscription(EventHandler<QuotaExceeded> handler);

    StreamKey streamKey() const override;

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
