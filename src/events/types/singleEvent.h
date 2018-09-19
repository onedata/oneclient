/**
 * @file singleEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_SINGLE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_SINGLE_EVENT_H

#include "event.h"

namespace one {
namespace client {
namespace events {

/**
 * @c SingleEvent class represents an abstract event that ignores aggregation.
 * It should be processed by streams that do not rely on events aggregation
 * and handles each event separately.
 */
class SingleEvent : public Event {
public:
    /**
     * Returns a default, empty aggregation key.
     * @see Event::aggregationKey()
     */
    const AggregationKey &aggregationKey() const override;

    /**
     * Skip aggregation of the provided event.
     * @param event An event to be aggregated.
     */
    void aggregate(EventPtr<> event);

private:
    AggregationKey m_aggregationKey;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_SINGLE_EVENT_H
