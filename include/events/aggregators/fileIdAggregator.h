/**
* @file fileIdAggregator.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_AGGREGATORS_FILE_ID_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_FILE_ID_AGGREGATOR_H

#include "aggregator.h"

#include <map>
#include <list>
#include <algorithm>
#include <functional>

namespace one {
namespace client {
namespace events {

/**
* The FileIdAggregator class represents an aggregator that aggregates events
* with the same file ID.
*/
template <class EventType>
class FileIdAggregator : public Aggregator<EventType> {
public:
    /**
    * @copydoc Aggregator::aggregate(const EventType &event)
    * @c FileIdAggregator aggregates events with the same file ID. Events
    * with different file ID will not be aggregated and returned as separate
    * entities due to @c reset method call. Return value is an overall
    * aggregation result.
    */
    const EventType &aggregate(const EventType &event) override;

    /**
    * @copydoc Aggregator::all()
    */
    virtual const EventType &all() const override;

    /**
    * @copydoc Aggregator::reset()
    * @c FileIdAggregator returns list of aggregation results for events with
    * different file ID.
    */
    std::list<EventType> reset() override;

private:
    EventType m_all;
    std::map<std::string, EventType> m_eventsByFileId;
};

template <class EventType>
const EventType &FileIdAggregator<EventType>::aggregate(const EventType &event)
{
    m_all += event;
    auto insertionResult =
        m_eventsByFileId.insert(std::make_pair(event.fileId(), event));
    if (!insertionResult.second)
        insertionResult.first->second += event;
    return m_all;
}

template <class EventType>
const EventType &FileIdAggregator<EventType>::all() const
{
    return m_all;
}

template <class EventType>
std::list<EventType> FileIdAggregator<EventType>::reset()
{
    std::list<EventType> events;
    std::transform(
        m_eventsByFileId.begin(), m_eventsByFileId.end(),
        std::back_inserter(events),
        std::bind(&std::map<std::string, EventType>::value_type::second,
                  std::placeholders::_1));
    m_all = EventType{};
    m_eventsByFileId.clear();
    return events;
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_FILE_ID_AGGREGATOR_H