/**
 * @file remoteSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOTE_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOTE_SUBSCRIPTION_H

#include "subscription.h"

namespace one {
namespace client {
namespace events {

class RemoteSubscription : public Subscription {
public:
    virtual ~RemoteSubscription() = default;

    SubscriptionHandlePtr createHandle(std::int64_t subscriptionId,
        Streams &streams, SequencerStream &stream) const override;

    virtual ProtoSubscriptionPtr serialize() const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOTE_SUBSCRIPTION_H
