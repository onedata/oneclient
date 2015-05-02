/**
* @file eventStream.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_STREAMS_EVENT_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_EVENT_STREAM_H

#include "context.h"
#include "scheduler.h"
#include "eventCommunicator.h"
#include "events/aggregators/nullAggregator.h"
#include "events/aggregators/fileIdAggregator.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"

#include <boost/asio/strand.hpp>

#include <set>
#include <chrono>
#include <memory>
#include <vector>
#include <cstdint>
#include <functional>
#include <sys/types.h>

namespace one {
namespace client {
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
    void push(EventType event);

    /**
    * Adds a subscription for events of type @c EventType.
    * @param subscription An instance of subscription of type @c
    * SubscriptionType to be added.
    * @return Id of subscription.
    */
    uint64_t addSubscription(typename EventType::Subscription subscription);

    /**
    * Removes a subscription for events of type @c EventType.
    * @param subscription An instance of subscription of type @c
    * SubscriptionType to be removed.
    */
    void removeSubscription(typename EventType::Subscription subscription);

private:
    bool isEmissionRuleSatisfied(const EventType &event);
    void emit();
    void periodicEmission();
    void resetPeriodicEmission();

    std::multiset<size_t> m_counterThresholds;
    std::multiset<std::chrono::milliseconds> m_timeThresholds;
    std::multiset<size_t> m_sizeThresholds;

    std::weak_ptr<Context> m_context;
    std::shared_ptr<EventCommunicator> m_communicator;
    std::unique_ptr<Aggregator<EventType>> m_aggregator;

    boost::asio::strand m_streamStrand;
    std::function<void()> m_cancelPeriodicEmission = [] {};
};

template <class EventType>
EventStream<EventType>::EventStream(std::weak_ptr<Context> context,
    std::shared_ptr<EventCommunicator> communicator)
    : m_context{std::move(context)}
    , m_communicator{std::move(communicator)}
    , m_aggregator{std::make_unique<NullAggregator<EventType>>()}
    , m_streamStrand{m_context.lock()->scheduler()->getIoService()}
{
}

template <class EventType> void EventStream<EventType>::push(EventType event)
{
    m_streamStrand.post([ this, event = std::move(event) ] {
        const EventType &aggregatedEvent =
            m_aggregator->aggregate(std::move(event));
        if (isEmissionRuleSatisfied(aggregatedEvent))
            emit();
    });
}

template <class EventType>
uint64_t EventStream<EventType>::addSubscription(
    typename EventType::Subscription subscription)
{
    LOG(INFO) << "Adding event subscripton: " << subscription.toString();

    uint64_t id = subscription.id();
    m_streamStrand.post([ this, subscription = std::move(subscription) ] {
        bool isAnyThresholdSet = true;
        if (m_counterThresholds.size() == 0 && m_timeThresholds.size() == 0 &&
            m_sizeThresholds.size() == 0) {
            isAnyThresholdSet = false;
        }

        auto minTimeThreshold = std::chrono::milliseconds::max();
        if (!m_timeThresholds.empty())
            minTimeThreshold = *m_timeThresholds.begin();

        if (subscription.counterThreshold())
            m_counterThresholds.insert(subscription.counterThreshold().get());
        if (subscription.timeThreshold())
            m_timeThresholds.insert(subscription.timeThreshold().get());
        if (subscription.sizeThreshold())
            m_sizeThresholds.insert(subscription.sizeThreshold().get());

        if (isEmissionRuleSatisfied(m_aggregator->all()))
            emit();
        else if (!m_timeThresholds.empty() &&
            minTimeThreshold != *m_timeThresholds.begin())
            resetPeriodicEmission();

        if (!isAnyThresholdSet &&
            (m_counterThresholds.size() != 0 || m_timeThresholds.size() != 0 ||
                m_sizeThresholds.size() != 0))
            m_aggregator = std::make_unique<FileIdAggregator<EventType>>();
    });
    return id;
}

template <class EventType>
void EventStream<EventType>::removeSubscription(
    typename EventType::Subscription subscription)
{
    LOG(INFO) << "Removing event subscripton: " << subscription.toString();

    m_streamStrand.post([ this, subscription = std::move(subscription) ] {
        if (subscription.counterThreshold())
            m_counterThresholds.erase(m_counterThresholds.find(
                subscription.counterThreshold().get()));
        if (subscription.timeThreshold())
            m_timeThresholds.erase(
                m_timeThresholds.find(subscription.timeThreshold().get()));
        if (subscription.sizeThreshold())
            m_sizeThresholds.erase(
                m_sizeThresholds.find(subscription.sizeThreshold().get()));

        if (m_counterThresholds.size() == 0 && m_timeThresholds.size() == 0 &&
            m_sizeThresholds.size() == 0)
            m_aggregator = std::make_unique<NullAggregator<EventType>>();
    });
}

template <class EventType>
bool EventStream<EventType>::isEmissionRuleSatisfied(const EventType &event)
{
    return (!m_counterThresholds.empty() &&
               event.counter() >= *m_counterThresholds.begin()) ||
        (!m_sizeThresholds.empty() &&
               event.size() >= *m_sizeThresholds.begin());
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
    m_streamStrand.post([this] { emit(); });
}

template <class EventType> void EventStream<EventType>::resetPeriodicEmission()
{
    m_cancelPeriodicEmission();
    if (!m_timeThresholds.empty())
        m_cancelPeriodicEmission =
            m_context.lock()->scheduler()->schedule(*m_timeThresholds.begin(),
                std::bind(&EventStream<EventType>::periodicEmission, this));
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_EVENT_STREAM_H
