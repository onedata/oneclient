/**
 * @file fileAccessedEvent.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_ACCESSED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_ACCESSED_EVENT_H

#include "event.h"
#include "messages/serverMessage.h"

#include <string>
#include <memory>

namespace one {
namespace clproto {
class FileAccessedEvent;
}
namespace client {
namespace events {

class FileAccessedSubscription;

/**
 * @c FileAccessedEvent class represents an event of accessing file.
 */
class FileAccessedEvent : public Event {
public:
    using EventPtr = std::unique_ptr<FileAccessedEvent>;
    using Key = std::string;
    using ProtocolMessage = clproto::FileAccessedEvent;
    using Subscription = FileAccessedSubscription;

    /**
     * Constructor.
     * @param fileUuid of file associated with the file accessed event.
     * @param openCount Number of open operation on file
     * @param releaseCount Number of release operation on file
     * event.
     */
    FileAccessedEvent(
        std::string fileUuid, uint64_t openCount, uint64_t releaseCount);

    /**
     * @return Value that distinguish @c this file accessed event from other
     * events, i.e. events with the same key can be aggregated.
     * @see @c fileAccessedEvent::Key.
     */
    const Key key() const;

    /**
     * @return ID of file associated with the file accessed event.
     */
    const std::string &fileUuid() const;

    /**
     * @return Number of open operation on file associated with the file
     * accessed event.
     */
    const uint64_t &openCount() const;

    /**
     * @return Number of release operation on file associated with the file
     * accessed event.
     */
    const uint64_t &releaseCount() const;

    /**
     * Aggregates @c this event with an other file accessed event.
     * Aggregation is done by:
     * - addition of events' counters
     * - addition of file open counters
     * - addition of file release counters
     * @param event file accessed event to be aggregated.
     */
    void aggregate(EventPtr event);

    std::string toString() const override;

    std::unique_ptr<ProtocolEventMessage> serializeAndDestroy() override;

protected:
    std::string m_fileUuid;
    uint64_t m_openCount;
    uint64_t m_releaseCount;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_ACCESSED_EVENT_H
