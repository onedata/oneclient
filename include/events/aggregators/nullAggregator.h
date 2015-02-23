/**
* @file nullAggregator.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_AGGREGATORS_NULL_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_NULL_AGGREGATOR_H

#include "aggregator.h"

#include <list>

namespace one {
namespace client {
namespace events {

/**
* The NullAggregator class is an implementation of Null Object design pattern.
* The aggregator ignores provided elements.
*/
template <class EventType> class NullAggregator : public Aggregator<EventType> {
public:
    /**
    * @copydoc Aggregator::aggregate(const EventType &event)
    * @c NullAggregator ignores provided @p event and always returns
    * aggregation identity element.
    */
    const EventType &aggregate(const EventType &event) override;

    /**
    * @copydoc Aggregator::all()
    * @c NullAggregator always returns aggregation identity element.
    */
    virtual const EventType &all() const override;

    /**
    * @copydoc Aggregator::reset()
    * @c NullAggregator always return an empty list of aggregated events.
    */
    std::list<EventType> reset() override;

private:
    EventType m_all;
};

template <class EventType>
const EventType &NullAggregator<EventType>::aggregate(const EventType &event)
{
    return m_all;
}

template <class EventType>
const EventType &NullAggregator<EventType>::all() const
{
    return m_all;
}

template <class EventType>
std::list<EventType> NullAggregator<EventType>::reset()
{
    return std::list<EventType>();
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_NULL_AGGREGATOR_H