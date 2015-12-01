/**
 * @file eventCounterAggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_AGGREGATORS_EVENT_COUNTER_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_EVENT_COUNTER_AGGREGATOR_H

#include <cstddef>
#include <set>

namespace one {
namespace client {
namespace events {

/**
 * @c EventCounterAggregator is responsible for aggregating events' counters
 * and triggering emission when counter threshold is exceeded.
 */
template <class LowerLayer> class EventCounterAggregator : public LowerLayer {
public:
    using EventPtr = typename LowerLayer::EventPtr;
    using OnTriggerCallback = typename LowerLayer::OnTriggerCallback;
    using SubscriptionPtr = typename LowerLayer::SubscriptionPtr;

    using LowerLayer::LowerLayer;
    virtual ~EventCounterAggregator() = default;

    /**
     * Sets @c OnTriggerCallback which sets aggregation counter to zero.
     */
    void initializeAggregation();

    /**
     * Wraps lower layer's @c process.
     * Increments aggregation counter by @c event counter. Triggers events
     * emission if aggregation counter becomes greater than or equal to the
     * lowest subscriptions' counter threshold.
     * @see EventHandler::process()
     */
    void process(EventPtr event);

    /**
     * Wraps lower layer's @c subscribe.
     * Stores subscription's counter threshold and triggers events emission if
     * aggregation counter is greater than or equal to the added counter
     * threshold.
     * @see SubscriptionHandler::subscribe()
     */
    typename LowerLayer::UnsubscribeHandler subscribe(
        SubscriptionPtr subscription);

    /**
     * Wraps lower layer's @c setOnTriggerCallback.
     * Sets aggregation counter to zero.
     * @see EventHandler::setOnTriggerCallback()
     */
    void setOnTriggerCallback(OnTriggerCallback callback);

private:
    std::size_t m_counter = 0;
    std::multiset<std::size_t> m_counterThresholds;
};

template <class LowerLayer>
void EventCounterAggregator<LowerLayer>::initializeAggregation()
{
    LowerLayer::setOnTriggerCallback([this] { m_counter = 0; });
}

template <class LowerLayer>
void EventCounterAggregator<LowerLayer>::process(EventPtr event)
{
    m_counter += event->counter();
    if (!m_counterThresholds.empty() &&
        *m_counterThresholds.begin() <= m_counter)
        LowerLayer::trigger(std::move(event));
    else
        LowerLayer::process(std::move(event));
}

template <class LowerLayer>
typename LowerLayer::UnsubscribeHandler
EventCounterAggregator<LowerLayer>::subscribe(SubscriptionPtr subscription)
{
    if (subscription->counterThreshold()) {
        auto counterThreshold = subscription->counterThreshold().get();
        m_counterThresholds.emplace(counterThreshold);

        if (counterThreshold <= m_counter)
            LowerLayer::trigger();

        auto handler = LowerLayer::subscribe(std::move(subscription));
        return [ =, handler = std::move(handler) ]
        {
            auto it = m_counterThresholds.find(counterThreshold);
            m_counterThresholds.erase(it);

            if (m_counterThresholds.empty())
                m_counter = 0;

            handler();
        };
    }
    else
        return LowerLayer::subscribe(std::move(subscription));
}

template <class LowerLayer>
void EventCounterAggregator<LowerLayer>::setOnTriggerCallback(
    OnTriggerCallback callback)
{
    LowerLayer::setOnTriggerCallback([ this, callback = std::move(callback) ] {
        m_counter = 0;
        callback();
    });
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_EVENT_COUNTER_AGGREGATOR_H
