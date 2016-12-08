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

/**
 * @c RemoteSubscription class represents an abstract subscription for events
 * that are produced by a remote producer. It provides an interface for concrete
 * remote subscriptions.
 */
class RemoteSubscription : public Subscription {
public:
    virtual ~RemoteSubscription() = default;

    /**
     * Creates a @c RemoteSubscriptionHandle instance.
     * @see Subscription::createHandle(std::int64_t subscriptionId, Streams
     * &streams, SequencerStream &stream)
     */
    SubscriptionHandlePtr createHandle(std::int64_t subscriptionId,
        Streams &streams, SequencerStream &stream) const override;

    /**
     * Creates Protocol Buffers message based on provided @c RemoteSubscription.
     * @return A serialized remote subscription.
     */
    virtual ProtoSubscriptionPtr serialize() const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOTE_SUBSCRIPTION_H
