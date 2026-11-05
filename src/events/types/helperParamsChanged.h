/**
 * @file helperParamsChanged.h
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_HELPER_PARAMS_CHANGED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_HELPER_PARAMS_CHANGED_EVENT_H

#include "event.h"

namespace one {
namespace clproto {
class HelperParamsChangedEvent;
} // namespace clproto
namespace client {
namespace events {

class HelperParamsChanged : public Event {
    using ProtocolMessage = clproto::HelperParamsChangedEvent;

public:
    /**
     * Constructor.
     * @param msg A Protocol Buffers message representing @c HelperParamsChanged
     * counterpart.
     */
    HelperParamsChanged(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    /**
     * Aggregation key value is equal to the UUID of storage associated with the
     * event.
     * @see Event::aggregationKey()
     */
    const AggregationKey &aggregationKey() const override;

    /**
     * @return A updated storage id.
     */
    const std::string &storageId() const;

    std::string toString() const override;

    /**
     * Aggregates @c *this event with the other event. Aggregation is done by
     * choosing the other, more recently updated, file attributes.
     * @param event An event to be aggregated.
     */
    void aggregate(EventPtr<HelperParamsChanged> event);

private:
    std::string m_storageId;
    AggregationKey m_aggregationKey;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_HELPER_PARAMS_CHANGED_EVENT_H
