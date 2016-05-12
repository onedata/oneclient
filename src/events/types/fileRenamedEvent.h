/**
 * @file fileRenamedEvent.h
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_RENAMED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_FILE_RENAMED_EVENT_H

#include "event.h"
#include "messages/serverMessage.h"

#include <string>
#include <memory>

namespace one {
namespace clproto {
class FileRenamedEvent;
}
namespace client {
namespace events {

class FileRenamedSubscription;

/**
 * @c FileRenamedEvent class represents an event of renaming file.
 */
class FileRenamedEvent : public Event {
public:
    using EventPtr = std::unique_ptr<FileRenamedEvent>;
    using Key = std::string;
    using ProtocolMessage = clproto::FileRenamedEvent;
    using Subscription = FileRenamedSubscription;

    /**
     * Constructor.
     * @param message fileRenamedEvent protocol message.
     */
    FileRenamedEvent(const ProtocolMessage &message);
    FileRenamedEvent(std::string fileUuid);

    /**
     * @return Value that distinguish @c this rename event from other rename
     * events,
     * i.e. rename events with the same key can be aggregated.
     * @see @c FileRenamedEvent::Key.
     */
    const Key &key() const;

    /**
     * @return Old ID of file associated with the rename event.
     */
    const std::string &oldUuid() const;

    /**
     * @return New ID of file associated with the rename event.
     */
    const std::string &newUuid() const;

    /**
     * Aggregates @c this event with an other event.
     * Aggregation is done by addition of events' counters
     * @param event FileRenamedEvent to be aggregated.
     */
    void aggregate(EventPtr event);

    std::string toString() const override;

    std::unique_ptr<ProtocolEventMessage> serializeAndDestroy() override;

protected:
    std::string m_oldUuid;
    std::string m_newUuid;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_RENAMED_EVENT_H
