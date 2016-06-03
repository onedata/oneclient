/**
 * @file fileRemovalEvent.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_REMOVE_FILE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_REMOVE_FILE_EVENT_H

#include "event.h"
#include "messages/serverMessage.h"

#include <memory>
#include <string>

namespace one {
namespace clproto {
class FileRemovalEvent;
}
namespace client {
namespace events {

class FileRemovalSubscription;

/**
 * @c FileRemovalEvent class represents an event of removing file.
 */
class FileRemovalEvent : public Event {
public:
    using EventPtr = std::unique_ptr<FileRemovalEvent>;
    using Key = std::string;
    using ProtocolMessage = clproto::FileRemovalEvent;
    using Subscription = FileRemovalSubscription;

    /**
     * Constructor.
     * @param message fileRemovalEvent protocol message.
     */
    FileRemovalEvent(const ProtocolMessage &message);
    FileRemovalEvent(std::string fileUuid);

    /**
     * @return Value that distinguish @c this remove event from other remove
     * events,
     * i.e. remove events with the same key can be aggregated.
     * @see @c FileRemovalEvent::Key.
     */
    const Key &key() const;

    /**
     * @return ID of file associated with the remove event.
     */
    const std::string &fileUuid() const;

    /**
     * Aggregates @c this event with an other event.
     * Aggregation is done by addition of events' counters
     * @param event FileRemovalEvent to be aggregated.
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

#endif // ONECLIENT_EVENTS_TYPES_REMOVE_FILE_EVENT_H
