/**
* @file aggregator.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H

#include <vector>

namespace one {
namespace client {
namespace events {

/**
* The Aggregator abstract class provides an interface for all aggregators.
*/
template <class EventType> class Aggregator {
public:
    virtual ~Aggregator() = default;

    /**
    * Aggregates an event and returns overall aggregation result.
    * @param event Event of type @c EventType to be aggregated. Events of type
    * @EventType
    * have to be aggregable using @c += operator.
    * @return Aggregation result.
    */
    virtual const EventType &aggregate(const EventType &event) = 0;

    /**
    * Returns overall aggregation result.
    * @return An event representing aggregation result.
    */
    virtual const EventType &all() const = 0;

    /**
    * Retrieves all aggregated events by emptying the aggregator.
    * @return Vector of aggregated events.
    */
    virtual std::vector<EventType> reset() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H