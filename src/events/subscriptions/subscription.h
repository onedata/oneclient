/**
 * @file subscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H

#include "events/declarations.h"

#include <string>

namespace one {
namespace client {
namespace events {

/**
 * @c Subscription class represents an abstract subscription for events that
 * occure in the system. It provides an interface for concrete subscriptions.
 */
class Subscription {
public:
    virtual ~Subscription() = default;

    /**
     * Defines which stream should process events associated with this
     * subscription.
     * @return A @c StreamKey that identifies stream responsible for handling
     * events associated with this subscription.
     */
    virtual StreamKey streamKey() const = 0;

    /**
     * Creates a stream responsible for processing event associated with this
     * subscription.
     * @param manager A @c Manager instance.
     * @param seqManager A @c SequencerManager instance.
     * @param scheduler A @c Scheduler instance.
     * @return A created @c Stream instance.
     */
    virtual StreamPtr createStream(Manager &manager,
        SequencerManager &seqManager, Scheduler &scheduler) const = 0;

    /**
     * Creates a handle responsible for deletion of the event stream when the
     * last subscription is cancelled and possibly for sending subscription
     * cancellation message to the remote subscriber.
     * @param subscriptionId An ID of a subscription associated with this
     * handle.
     * @param streams A collection of existing event streams.
     * @param stream An @c SequencerStream instance.
     * @return A @c SubscriptionHandle instance.
     */
    virtual SubscriptionHandlePtr createHandle(std::int64_t subscriptionId,
        Streams &streams, SequencerStream &stream) const;

    /**
     * Provides a human-readable subscription description.
     * @return A subscription description.
     */
    virtual std::string toString() const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H
