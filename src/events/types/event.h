/**
 * @file event.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_EVENT_H
#define ONECLIENT_EVENTS_TYPES_EVENT_H

#include "events/declarations.h"

namespace one {
namespace client {
namespace events {

/**
 * @c Event class represents an abstract event that occured in the system.
 * It provides an interface for concrete events.
 */
class Event {
public:
    /**
     * Defines which stream should process this event.
     * @return A @c StreamKey that identifies stream responsible for handling
     * this event.
     */
    virtual StreamKey streamKey() const = 0;

    /**
     * Defines a value that distinguish two events in terms of aggregation, i.e.
     * events with that same aggregation key can be aggregated.
     * @return A value that distinguish two events in terms of aggregation.
     */
    virtual const AggregationKey &aggregationKey() const = 0;

    /**
     * Provides a human-readable event description.
     * @return An event description.
     */
    virtual std::string toString() const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_EVENT_H
