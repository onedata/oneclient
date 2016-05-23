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
#include "messages/fuse/fileRenamedEntry.h"
#include "messages/serverMessage.h"

#include <memory>
#include <string>
#include <vector>

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
    using FileRenamedEntry = messages::fuse::FileRenamedEntry;

    /**
     * Constructor.
     * @param message FileRenamedEvent protocol message.
     */
    FileRenamedEvent(const ProtocolMessage &message);

    /**
     * @return Value that distinguish @c this rename event from other rename
     * events,
     * i.e. rename events with the same key can be aggregated.
     * @see @c FileRenamedEvent::Key.
     */
    const Key &key() const;

    /**
     * @return Entry describing changes in renamed file.
     */
    const FileRenamedEntry &topEntry() const;

    /**
     * @return List of entries describing changes in children of renamed file.
     */
    const std::vector<FileRenamedEntry> &childEntries() const;

    /**
     * Aggregates @c this event with an other event.
     * Aggregation is done by addition of events' counters
     * @param event FileRenamedEvent to be aggregated.
     */
    void aggregate(EventPtr event);

    std::string toString() const override;

    std::unique_ptr<ProtocolEventMessage> serializeAndDestroy() override;

protected:
    FileRenamedEntry m_topEntry;
    std::vector<FileRenamedEntry> m_childEntries;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_RENAMED_EVENT_H
