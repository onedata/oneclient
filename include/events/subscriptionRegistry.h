/**
 * @file subscriptionRegistry.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTION_REGISTRY_H
#define ONECLIENT_EVENTS_SUBSCRIPTION_REGISTRY_H

#include <asio/io_service_strand.hpp>

#include <future>
#include <utility>
#include <functional>
#include <unordered_map>

namespace one {
namespace client {

class Context;

namespace events {

class EventSubscriptionCancellation;

/**
 * The SubscriptionRegistry class is responsible for storing events
 * subscriptions.
 */
class SubscriptionRegistry {
public:
    /**
     * Constructor.
     * @param context A @c Context instance used to acquire @c Scheduler IO
     * service.
     */
    SubscriptionRegistry(std::shared_ptr<Context> ctx);

    virtual ~SubscriptionRegistry() = default;

    /**
     * Adds subscription to the registry.
     * @param sub ID of subscription and subscription cancellation function.
     * @return Future set to 'true' if subscription was successfully added,
     * otherwise future set to 'false'.
     */
    virtual std::future<bool> add(
        std::pair<uint64_t, std::function<void()>> sub);

    /**
     * Removes subscription from the registry.
     * @param subCan Instance of @EventSubscriptionCancellation containing ID of
     * subscription to be cancelled and removed from registry.
     * @return Future set to 'true' if subscription was successfully removed
     * from the registry, otherwise future set to 'false'.
     */
    virtual std::future<bool> remove(EventSubscriptionCancellation subCan);

private:
    std::weak_ptr<Context> m_ctx;
    asio::io_service::strand m_strand;
    std::unordered_map<uint64_t, std::function<void()>> m_subById;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTION_REGISTRY_H
