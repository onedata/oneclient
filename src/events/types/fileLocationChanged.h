/**
 * @file fileLocationChanged.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_LOCATION_CHANGED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_LOCATION_CHANGED_EVENT_H

#include "event.h"

namespace one {
namespace clproto {
class FileLocationChangedEvent;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c FileLocationChanged class represents a change of file location in the
 * system.
 */
class FileLocationChanged : public Event {
    using ProtocolMessage = clproto::FileLocationChangedEvent;

public:
    /**
     * Constructor.
     * @param msg A Protocol Buffers message representing @c FileLocationChanged
     * counterpart.
     */
    FileLocationChanged(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    /**
     * Aggregation key value is equal to the UUID of a file associated with the
     * event.
     * @see Event::aggregationKey()
     */
    const AggregationKey &aggregationKey() const override;

    /**
     * @return A updated file location.
     */
    const FileLocation &fileLocation() const;

    std::string toString() const override;

    /**
     * Aggregates @c *this event with the other event. Aggregation is done by
     * choosing the other, more recently updated, file location.
     * @param event An event to be aggregated.
     */
    void aggregate(EventPtr<FileLocationChanged> event);

private:
    std::unique_ptr<FileLocation> m_fileLocation;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_LOCATION_CHANGED_EVENT_H
