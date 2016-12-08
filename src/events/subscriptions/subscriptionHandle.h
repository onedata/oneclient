/**
 * @file subscriptionHandle.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_HANDLE_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_HANDLE_H

#include "events/declarations.h"

namespace one {
namespace client {
namespace events {

/**
 * @c SubscriptionHandle class manages the lifetime of a subscription and is
 * responsible for an associated event stream removal when the last subscription
 * is cancelled.
 */
class SubscriptionHandle {
public:
    /**
     * Constructor.
     * @param streamKey A key of a stream associated with the handle by a
     * subscription.
     * @param streams A collection of existing event streams.
     */
    SubscriptionHandle(StreamKey streamKey, Streams &streams);

    /**
     * Removes an associated event stream when the last subscription is
     * cancelled.
     */
    virtual ~SubscriptionHandle();

private:
    StreamKey m_streamKey;
    Streams &m_streams;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_HANDLE_H
