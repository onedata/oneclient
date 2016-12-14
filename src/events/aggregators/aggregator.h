/**
 * @file aggregator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H
#define ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H

#include "events/declarations.h"

namespace one {
namespace client {
namespace events {

/**
 * @c Aggregator class represents an abstract event stream layer responsible for
 * homogeneous events aggregation. It provides an interface for concrete
 * aggregators.
 */
template <class T> class Aggregator {
public:
    virtual ~Aggregator() = default;

    /**
     * Aggregates an event.
     * @param event An event to be aggregated.
     */
    virtual void process(EventPtr<T> event) = 0;

    /**
     * Clears the container by returning aggregated events.
     * @return A collection of aggregated events.
     */
    virtual Events<T> flush() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_AGGREGATORS_AGGREGATOR_H
