/**
 * @file eventTimeAggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_AGGREGATORS_EVENT_TIME_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_EVENT_TIME_AGGREGATOR_H

#include "scheduler.h"

#include <chrono>
#include <functional>
#include <memory>
#include <set>

namespace one {

class Scheduler;

namespace client {
namespace events {

/**
 * @c EventTimeAggregator is responsible triggering periodic events emission.
 */
template <class LowerLayer, class Scheduler = one::Scheduler>
class EventTimeAggregator : public LowerLayer {
public:
    using OnTriggerCallback = typename LowerLayer::OnTriggerCallback;
    using PeriodicTriggerHandler = std::function<void()>;
    using SchedulerPtr = std::shared_ptr<Scheduler>;
    using SubscriptionPtr = typename LowerLayer::SubscriptionPtr;

    using LowerLayer::LowerLayer;
    virtual ~EventTimeAggregator() = default;

    /**
     * Sets @c OnTriggerCallback which resets periodic events emission trigger.
     */
    void initializeAggregation();

    /**
     * Wraps lower layer's @c subscribe.
     * Stores subscription's time threshold and reset periodic events emission
     * trigger if aggregation time is greater than or equal to the added time
     * threshold.
     * @see SubscriptionHandler::subscribe()
     */
    typename LowerLayer::UnsubscribeHandler subscribe(
        SubscriptionPtr subscription);

    /**
     * Sets the @c Scheduler which will be used to schedule execution of
     * periodic events emission handler.
     * @param scheduler @c Scheduler instance.
     */
    void setScheduler(SchedulerPtr scheduler);

    /**
     * Sets handler which will be called when time threshold exceeds.
     * @param handler Handler to be set.
     */
    void setPeriodicTriggerHandler(PeriodicTriggerHandler handler);

    /**
     * Wraps lower layer's @c setOnTriggerCallback.
     * Resets periodic events emission trigger.
     * @see EventHandler::setOnTriggerCallback()
     */
    void setOnTriggerCallback(OnTriggerCallback callback);

private:
    void periodicTrigger();
    void resetPeriodicTrigger();

    SchedulerPtr m_scheduler;
    PeriodicTriggerHandler m_periodicTriggerHandler;
    std::multiset<std::chrono::milliseconds> m_timeThresholds;
    std::function<void()> m_cancelPeriodicTrigger = [] {};
};

template <class LowerLayer, class Scheduler>
void EventTimeAggregator<LowerLayer, Scheduler>::initializeAggregation()
{
    LowerLayer::setOnTriggerCallback([this] { resetPeriodicTrigger(); });
}

template <class LowerLayer, class Scheduler>
typename LowerLayer::UnsubscribeHandler
EventTimeAggregator<LowerLayer, Scheduler>::subscribe(
    SubscriptionPtr subscription)
{
    if (subscription->timeThreshold()) {
        auto timeThreshold = subscription->timeThreshold().get();

        if (m_timeThresholds.empty() ||
            timeThreshold < *m_timeThresholds.begin()) {
            m_timeThresholds.emplace(timeThreshold);
            resetPeriodicTrigger();
        }
        else
            m_timeThresholds.emplace(timeThreshold);

        auto handler = LowerLayer::subscribe(std::move(subscription));

        return [
            this,
            handler = std::move(handler),
            timeThreshold = std::move(timeThreshold)
        ]
        {
            auto it = m_timeThresholds.find(timeThreshold);
            m_timeThresholds.erase(it);

            if (m_timeThresholds.empty())
                m_cancelPeriodicTrigger();

            handler();
        };
    }
    else
        return LowerLayer::subscribe(std::move(subscription));
}

template <class LowerLayer, class Scheduler>
void EventTimeAggregator<LowerLayer, Scheduler>::setScheduler(
    SchedulerPtr scheduler)
{
    m_scheduler = std::move(scheduler);
}

template <class LowerLayer, class Scheduler>
void EventTimeAggregator<LowerLayer, Scheduler>::setPeriodicTriggerHandler(
    PeriodicTriggerHandler handler)
{
    m_periodicTriggerHandler = std::move(handler);
}

template <class LowerLayer, class Scheduler>
void EventTimeAggregator<LowerLayer, Scheduler>::setOnTriggerCallback(
    OnTriggerCallback callback)
{
    LowerLayer::setOnTriggerCallback([ this, callback = std::move(callback) ] {
        resetPeriodicTrigger();
        callback();
    });
}

template <class LowerLayer, class Scheduler>
void EventTimeAggregator<LowerLayer, Scheduler>::resetPeriodicTrigger()
{
    m_cancelPeriodicTrigger();
    if (!m_timeThresholds.empty())
        m_cancelPeriodicTrigger = m_scheduler->schedule(
            *m_timeThresholds.begin(), m_periodicTriggerHandler);
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_EVENT_TIME_AGGREGATOR_H
