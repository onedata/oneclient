/**
 * @file subscriptionHandler.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTION_HANDLER_H
#define ONECLIENT_EVENTS_SUBSCRIPTION_HANDLER_H

#include "events/buffers/eventBufferMap.h"
#include "events/buffers/voidEventBuffer.h"
#include "events/subscriptions/subscriptionCancellation.h"

#include <memory>
#include <unordered_map>
#include <functional>

namespace one {
namespace client {
namespace events {

/**
 * @c SubscriptionHandler is responsible for handling subscriptions for
 * events.
 */
template <class LowerLayer> class SubscriptionHandler : public LowerLayer {
public:
    using EventT = typename LowerLayer::EventT;
    using Subscription = typename EventT::Subscription;
    using SubscriptionPtr = typename LowerLayer::SubscriptionPtr;
    using UnsubscribeHandler = std::function<void()>;

    using LowerLayer::LowerLayer;
    virtual ~SubscriptionHandler() = default;

    /**
     * Stores subscription and sets lower layer's @c EventBuffer to @c
     * EventBufferMap if there were no subscriptions previously.
     * @param subscripion A subscription to be added.
     * @return Unsubscribe handler which removes subscription. It will
     * also set lower layer's @c EventBuffer to @c VoidEventBuffer if the last
     * subscription was removed.
     */
    UnsubscribeHandler subscribe(SubscriptionPtr subscripion);

private:
    std::unordered_map<std::uint64_t, SubscriptionPtr> m_subscriptions;
};

template <class LowerLayer>
typename SubscriptionHandler<LowerLayer>::UnsubscribeHandler
SubscriptionHandler<LowerLayer>::subscribe(SubscriptionPtr subscription)
{
    if (!subscription->empty()) {
        if (m_subscriptions.empty())
            LowerLayer::setEventBuffer(
                std::make_unique<EventBufferMap<EventT>>());

        auto id = subscription->id();
        m_subscriptions.emplace(id, std::move(subscription));

        return [=] {
            m_subscriptions.erase(id);
            if (m_subscriptions.empty())
                LowerLayer::setEventBuffer(
                    std::make_unique<VoidEventBuffer<EventT>>());
            if (id < 0)
                LowerLayer::send(SubscriptionCancellation{id});
        };
    }
    else
        return [] {};
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTION_HANDLER_H
