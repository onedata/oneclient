/**
 * @file subscriptionRegistry.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "subscriptionRegistry.h"

namespace one {
namespace client {
namespace events {

std::int64_t SubscriptionRegistry::nextSubscriptionId()
{
    return -m_subscriptionId++;
}

bool SubscriptionRegistry::addUnsubscribeHandler(
    std::int64_t id, UnsubscribeHandler handler)
{
    typename decltype(m_handlers)::accessor acc;
    if (m_handlers.insert(acc, id)) {
        acc->second = std::move(handler);
        return true;
    }
    return false;
}

bool SubscriptionRegistry::removeSubscription(
    SubscriptionCancellation cancellation)
{
    typename decltype(m_handlers)::accessor acc;
    if (m_handlers.find(acc, cancellation.id())) {
        acc->second();
        m_handlers.erase(acc);
        return true;
    }
    return false;
}

bool SubscriptionRegistry::existSubscription(std::int64_t id) const
{
    typename decltype(m_handlers)::const_accessor acc;
    return m_handlers.find(acc, id);
}

} // namespace events
} // namespace client
} // namespace one
