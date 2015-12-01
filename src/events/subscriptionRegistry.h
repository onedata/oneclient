/**
 * @file subscriptionRegistry.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTION_REGISTRY_H
#define ONECLIENT_EVENTS_SUBSCRIPTION_REGISTRY_H

#include "events/subscriptions/subscriptionCancellation.h"

#include <tbb/concurrent_hash_map.h>

#include <atomic>
#include <memory>
#include <functional>

namespace one {
namespace client {
namespace events {

/**
 * @c SubscriptionRegistry is responsible for storing unsubscribe handlers
 * for each subscription. Instance of @c SubscriptionRegistry should be
 * shared between all event streams.
 */
class SubscriptionRegistry {
public:
    using UnsubscribeHandler = std::function<void()>;

    virtual ~SubscriptionRegistry() = default;

    /**
     * Generates consecutive IDs for client subscriptions.
     * Client subscriptions' IDs are negative.
     * @return Subscription ID.
     */
    std::int64_t nextSubscriptionId();

    /**
     * Adds unsubscribe handler if there is no other handler associated with
     * provided ID and returns true. If other handler is already associated with
     * provided ID does not overwrite it, but executes provided handler and
     * returns false.
     * @param id ID of subscription associated with the handler.
     * @param handler Unsubscribe handler.
     * @return 'true' if handler was successfully added, otherwise 'false'.
     */
    virtual bool addUnsubscribeHandler(
        std::int64_t id, UnsubscribeHandler handler);

    /**
     * Removes subscription by retrieving, executing and removing unsubscribe
     * handler.
     * @param subscription @c Subscription to be removed.
     * @return 'true' if subscription was successfully removed, otherwise
     * 'false'.
     */
    virtual bool removeSubscription(SubscriptionCancellation cancellation);

    /**
     * Checks whether subscription with provided ID exists.
     * @param id ID of subscription to be checked.
     * @return 'true' if subscription exists, otherwise 'false'
     */
    bool existSubscription(std::int64_t id) const;

private:
    std::atomic<std::int64_t> m_subscriptionId{1};
    tbb::concurrent_hash_map<std::int64_t, UnsubscribeHandler> m_handlers;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTION_REGISTRY_H
