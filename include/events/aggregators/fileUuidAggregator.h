/**
 * @file fileUuidAggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_AGGREGATORS_FILE_UUID_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_FILE_UUID_AGGREGATOR_H

#include "aggregator.h"

#include <vector>
#include <algorithm>
#include <functional>
#include <unordered_map>

namespace one {
namespace client {
namespace events {

/**
 * The FileIdAggregator class represents an aggregator that aggregates events
 * with the same file UUID.
 */
template <class EventType>
class FileUuidAggregator : public Aggregator<EventType> {
public:
    /**
     * @copydoc Aggregator::aggregate(const EventType &event)
     * @c FileUuidAggregator aggregates events with the same file UUID. Events
     * with different file UUID will not be aggregated and returned as separate
     * entities due to @c reset method call. Return value is an overall
     * aggregation result.
     */
    const EventType &aggregate(EventType event) override;

    virtual const EventType &all() const override;

    /**
     * @copydoc Aggregator::reset()
     * @c FileUuidAggregator returns vector of aggregation results for events
     * with different file UUID.
     */
    std::vector<EventType> reset() override;

private:
    EventType m_all;
    std::unordered_map<std::string, EventType> m_eventsByFileUuid;
};

template <class EventType>
const EventType &FileUuidAggregator<EventType>::aggregate(EventType event)
{
    m_all += event;
    auto searchResult = m_eventsByFileUuid.find(event.fileUuid());
    if (searchResult != m_eventsByFileUuid.end())
        searchResult->second += event;
    else
        m_eventsByFileUuid.emplace(event.fileUuid(), std::move(event));
    return m_all;
}

template <class EventType>
const EventType &FileUuidAggregator<EventType>::all() const
{
    return m_all;
}

template <class EventType>
std::vector<EventType> FileUuidAggregator<EventType>::reset()
{
    std::vector<EventType> events;
    std::transform(
        m_eventsByFileUuid.begin(), m_eventsByFileUuid.end(),
        std::back_inserter(events),
        std::bind(
            &std::unordered_map<std::string, EventType>::value_type::second,
            std::placeholders::_1));
    m_all = EventType{};
    m_eventsByFileUuid.clear();
    return events;
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_FILE_UUID_AGGREGATOR_H
