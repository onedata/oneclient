/**
* @file eventStream.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_STREAMS_EVENT_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_EVENT_STREAM_H

#include "scheduler.h"
#include "eventCommunicator.h"
#include "events/aggregators/nullAggregator.h"
#include "events/aggregators/fileIdAggregator.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"

#include <set>
#include <mutex>
#include <chrono>
#include <memory>
#include <vector>
#include <cstdint>
#include <functional>
#include <sys/types.h>

namespace one {
namespace client {

class Context;

namespace events {

/**
* The EventStream class is responsible aggregation and emission of events of
* type @c EventType.
*/
template <class EventType> class EventStream {
public:
    /**
    * Constructor.
    * @param context A @c Context instance used to acquire @c Scheduler
    * instance which is later used to schedule periodic events emission from the
    * stream.
    * @param communicator An @c EventCommunicator instance to which emitted
    * event are forwarded.
    */
    EventStream(std::weak_ptr<Context> context,
                std::shared_ptr<EventCommunicator> communicator);

    /**
    * Pushes an event of type @c EventType to the stream.
    * @param event An event of type @c EventType to be pushed to the @c
    * EventStream.
    */
    void push(const EventType &event);

    /**
    * Adds a subscription for events of type @c EventType.
    * @param subscription An instance of subscription of type @c
    * SubscriptionType to be added.
    * @return Subscription ID.
    */
    uint64_t
    addSubscription(const typename EventType::subscription &subscription);

    /**
    * Removes a subscription for events of type @c EventType.
    * @param subscription An instance of subscription of type @c
    * SubscriptionType to removed.
    */
    void
    removeSubscription(const typename EventType::subscription &subscription);

private:
    bool isEmissionRuleSatisfied(const EventType &event);
    void emit();
    void periodicEmission();
    void resetPeriodicEmission();

    std::multiset<size_t> m_counterThresholds{SIZE_MAX};
    std::multiset<std::chrono::milliseconds> m_timeThresholds{
        std::chrono::milliseconds::max()};
    std::multiset<size_t> m_sizeThresholds{SIZE_MAX};

    std::weak_ptr<Context> m_context;
    std::shared_ptr<EventCommunicator> m_communicator;
    std::unique_ptr<Aggregator<EventType>> m_aggregator;

    std::mutex m_streamMutex;
    std::function<void()> m_cancelPeriodicEmission = [] {};
};

template <class EventType>
EventStream<EventType>::EventStream(
    std::weak_ptr<Context> context,
    std::shared_ptr<EventCommunicator> communicator)
    : m_context{std::move(context)}
    , m_communicator{std::move(communicator)}
    , m_aggregator{std::make_unique<NullAggregator<EventType>>()}
{
}

template <class EventType>
void EventStream<EventType>::push(EventType const &event)
{
    std::lock_guard<std::mutex> guard{m_streamMutex};
    const EventType &aggregatedEvent = m_aggregator->aggregate(event);
    if (isEmissionRuleSatisfied(aggregatedEvent))
        emit();
}

template <class EventType>
uint64_t EventStream<EventType>::addSubscription(
    const typename EventType::subscription &subscription)
{
    std::lock_guard<std::mutex> guard{m_streamMutex};
    auto timeThreshold = *m_timeThresholds.begin();
    m_counterThresholds.insert(subscription.m_counterThreshold);
    m_timeThresholds.insert(subscription.m_timeThreshold);
    m_sizeThresholds.insert(subscription.m_sizeThreshold);

    if (isEmissionRuleSatisfied(m_aggregator->all()))
        emit();
    else if (timeThreshold != *m_timeThresholds.begin())
        resetPeriodicEmission();

    if (m_counterThresholds.size() == 2)
        m_aggregator = std::make_unique<FileIdAggregator<EventType>>();

    return subscription.m_id;
}

template <class EventType>
void EventStream<EventType>::removeSubscription(
    const typename EventType::subscription &subscription)
{
    std::lock_guard<std::mutex> guard{m_streamMutex};
    m_counterThresholds.erase(subscription.m_counterThreshold);
    m_timeThresholds.erase(subscription.m_timeThreshold);
    m_sizeThresholds.erase(subscription.m_sizeThreshold);
    if (m_counterThresholds.size() == 1)
        m_aggregator = std::make_unique<NullAggregator<EventType>>();
}

template <class EventType>
bool EventStream<EventType>::isEmissionRuleSatisfied(EventType const &event)
{
    return event.counter() >= *m_counterThresholds.begin() ||
           event.size() >= *m_sizeThresholds.begin();
}

template <class EventType> void EventStream<EventType>::emit()
{
    std::vector<EventType> events = m_aggregator->reset();
    for (const EventType &event : events)
        m_communicator->send(event);
    resetPeriodicEmission();
}

template <class EventType> void EventStream<EventType>::periodicEmission()
{
    std::lock_guard<std::mutex> guard{m_streamMutex};
    emit();
}

template <class EventType> void EventStream<EventType>::resetPeriodicEmission()
{
    m_cancelPeriodicEmission();
    m_cancelPeriodicEmission = m_context.lock()->scheduler()->schedule(
        *m_timeThresholds.begin(),
        std::bind(&EventStream<EventType>::periodicEmission, this));
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_EVENT_STREAM_H