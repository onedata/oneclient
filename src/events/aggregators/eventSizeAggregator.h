/**
 * @file eventSizeAggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_AGGREGATORS_EVENT_SIZE_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_EVENT_SIZE_AGGREGATOR_H

#include <cstddef>
#include <set>

namespace one {
namespace client {
namespace events {

/**
 * @c EventSizeAggregator is responsible for aggregating events' sizes and
 * triggering emission when size threshold is exceeded.
 */
template <class LowerLayer> class EventSizeAggregator : public LowerLayer {
public:
    using EventPtr = typename LowerLayer::EventPtr;
    using OnTriggerCallback = typename LowerLayer::OnTriggerCallback;
    using SubscriptionPtr = typename LowerLayer::SubscriptionPtr;

    using LowerLayer::LowerLayer;
    virtual ~EventSizeAggregator() = default;

    /**
     * Sets @c OnTriggerCallback which sets aggregation size to zero.
     */
    void initializeAggregation();

    /**
     * Wraps lower layer's @c process.
     * Increments aggregation size by @c event size. Triggers events emission if
     * aggregation size becomes greater than or equal to the lowest
     * subscriptions' size threshold.
     * @see EventHandler::process()
     */
    void process(EventPtr event);

    /**
     * Wraps lower layer's @c subscribe.
     * Stores subscription's size threshold and triggers events emission if
     * aggregation size is greater than or equal to the added size threshold.
     * @see SubscriptionHandler::subscribe()
     */
    typename LowerLayer::UnsubscribeHandler subscribe(
        SubscriptionPtr subscription);

    /**
     * Wraps lower layer's @c setOnTriggerCallback.
     * Sets aggregation size to zero.
     * @see EventHandler::setOnTriggerCallback()
     */
    void setOnTriggerCallback(OnTriggerCallback callback);

private:
    std::size_t m_size = 0;
    std::multiset<std::size_t> m_sizeThresholds;
};

template <class LowerLayer>
void EventSizeAggregator<LowerLayer>::initializeAggregation()
{
    LowerLayer::setOnTriggerCallback([this] { m_size = 0; });
}

template <class LowerLayer>
void EventSizeAggregator<LowerLayer>::process(EventPtr event)
{
    m_size += event->size();
    if (!m_sizeThresholds.empty() && *m_sizeThresholds.begin() <= m_size)
        LowerLayer::trigger(std::move(event));
    else
        LowerLayer::process(std::move(event));
}

template <class LowerLayer>
typename LowerLayer::UnsubscribeHandler
EventSizeAggregator<LowerLayer>::subscribe(SubscriptionPtr subscription)
{
    if (subscription->sizeThreshold()) {
        auto sizeThreshold = subscription->sizeThreshold().get();
        m_sizeThresholds.emplace(sizeThreshold);

        if (sizeThreshold <= m_size)
            LowerLayer::trigger();

        auto handler = LowerLayer::subscribe(std::move(subscription));
        return [ =, handler = std::move(handler) ]
        {
            auto it = m_sizeThresholds.find(sizeThreshold);
            m_sizeThresholds.erase(it);

            if (m_sizeThresholds.empty())
                m_size = 0;

            handler();
        };
    }
    else
        return LowerLayer::subscribe(std::move(subscription));
}

template <class LowerLayer>
void EventSizeAggregator<LowerLayer>::setOnTriggerCallback(
    OnTriggerCallback callback)
{
    LowerLayer::setOnTriggerCallback([ this, callback = std::move(callback) ] {
        m_size = 0;
        callback();
    });
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_EVENT_SIZE_AGGREGATOR_H
