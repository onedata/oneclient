/**
 * @file fileAttrChanged.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_ATTR_CHANGED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_ATTR_CHANGED_EVENT_H

#include "event.h"

namespace one {
namespace clproto {
class FileAttrChangedEvent;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c FileAttrChanged class represents a change of file attributes in the
 * system.
 */
class FileAttrChanged : public Event {
    using ProtocolMessage = clproto::FileAttrChangedEvent;

public:
    /**
     * Constructor.
     * @param msg A Protocol Buffers message representing @c FileAttrChanged
     * counterpart.
     */
    FileAttrChanged(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    /**
     * Aggregation key value is equal to the UUID of a file associated with the
     * event.
     * @see Event::aggregationKey()
     */
    const AggregationKey &aggregationKey() const override;

    /**
     * @return A updated file attributes.
     */
    const FileAttr &fileAttr() const;

    std::string toString() const override;

    /**
     * Aggregates @c *this event with the other event. Aggregation is done by
     * choosing the other, more recently updated, file attributes.
     * @param event An event to be aggregated.
     */
    void aggregate(EventPtr<FileAttrChanged> event);

private:
    std::unique_ptr<FileAttr> m_fileAttr;
    AggregationKey m_aggregationKey;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_ATTR_CHANGED_EVENT_H
