/**
 * @file fileRemovedEvent.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_REMOVED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_REMOVED_EVENT_H

#include "event.h"
#include "messages/serverMessage.h"

#include <memory>
#include <string>

namespace one {
namespace clproto {
class FileRemovedEvent;
}
namespace client {
namespace events {

class FileRemovedSubscription;

/**
 * @c FileRemovedEvent class represents an event of removing file.
 */
class FileRemovedEvent : public Event {
public:
    using EventPtr = std::unique_ptr<FileRemovedEvent>;
    using Key = std::string;
    using ProtocolMessage = clproto::FileRemovedEvent;
    using Subscription = FileRemovedSubscription;

    /**
     * Constructor.
     * @param message fileRemovedEvent protocol message.
     */
    FileRemovedEvent(const ProtocolMessage &message);
    FileRemovedEvent(std::string fileUuid);

    /**
     * @return Value that distinguish @c this file removal event from other
     * events, i.e. events with the same key can be aggregated.
     * @see @c FileRemovedEvent::Key.
     */
    const Key &key() const;

    /**
     * @return ID of file associated with the file removal event.
     */
    const std::string &fileUuid() const;

    /**
     * Aggregates @c this event with an other event.
     * Aggregation is done by addition of events' counters
     * @param event FileRemovedEvent to be aggregated.
     */
    void aggregate(EventPtr event);

    std::string toString() const override;

    std::unique_ptr<ProtocolEventMessage> serializeAndDestroy() override;

protected:
    std::string m_fileUuid;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_REMOVED_EVENT_H
