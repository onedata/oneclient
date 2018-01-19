/**
 * @file keyAggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_AGGREGATORS_KEY_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_KEY_AGGREGATOR_H

#include "aggregator.h"
#include "events/types/event.h"
#include "logging.h"

#include <unordered_map>

namespace one {
namespace client {
namespace events {

/**
 * @c KeyAggregator is responsible for aggregation of events by aggregation key.
 * Whenever an event is processed by the @c KeyAggregator it is either merged
 * with an event, having the same aggregation key, already stored in the
 * aggregator or saved under its aggregation key in the aggregator.
 */
template <class T> class KeyAggregator : public Aggregator<T> {
public:
    /**
     * Processes an event by merging it with an event, having the same
     * aggregation key, already stored in the aggregator or saves  it under its
     * aggregation key.
     * @see Aggregator::process(EventPtr<T> event)
     */
    void process(EventPtr<T> event) override;

    /**
     * Returns a container of aggregated events in an unspecified order.
     * @see Aggregator::flush()
     */
    Events<T> flush() override;

private:
    std::unordered_map<AggregationKey, EventPtr<T>> m_events;
};

template <class T> void KeyAggregator<T>::process(EventPtr<T> event)
{
    LOG_FCALL();

    auto it = m_events.find(event->aggregationKey());
    if (it != m_events.end()) {
        LOG_DBG(2) << "Aggregating event " << event->toString()
                   << " with existing aggregation key "
                   << event->aggregationKey();
        it->second->aggregate(std::move(event));
    }
    else {
        auto key = event->aggregationKey();
        LOG_DBG(2) << "Aggregating event " << event->toString()
                   << " with new aggregation key " << event->aggregationKey();
        m_events.emplace(std::move(key), std::move(event));
    }
}

template <class T> Events<T> KeyAggregator<T>::flush()
{
    LOG_FCALL();

    Events<T> events;
    for (auto it = m_events.begin(); it != m_events.end(); ++it) {
        LOG_DBG(1) << "Emitting event: " << it->second->toString();
        events.emplace_back(std::move(it->second));
    }
    m_events.clear();
    return events;
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_KEY_AGGREGATOR_H
