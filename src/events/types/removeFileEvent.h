/**
 * @file removeFileEvent.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_REMOVE_FILE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_REMOVE_FILE_EVENT_H

#include "event.h"
#include "messages/serverMessage.h"

#include <boost/icl/interval_map.hpp>

#include <sys/types.h>

#include <string>
#include <memory>

namespace one {
namespace clproto {
class RemoveFileEvent;
}
namespace client {
namespace events {

class RemoveFileSubscription;

/**
 * @c RemoveFileEvent class represents an event of removing file
 */
class RemoveFileEvent : public Event {
public:
    using EventPtr = std::unique_ptr<RemoveFileEvent>;
    using Key = std::string;
    using ProtocolMessage = clproto::RemoveFileEvent;
    using Subscription = RemoveFileSubscription;

    /**
     * Constructor.
     * @param message removeFileEvent protocol message.
     */
    RemoveFileEvent(const ProtocolMessage &message);
    RemoveFileEvent(std::string fileUuid);

    /**
     * @return Value that distinguish @c this remove event from other remove
     * events,
     * i.e. remove events with the same key can be aggregated.
     * @see @c RemoveFileEvent::Key.
     */
    const Key &key() const;

    /**
     * @return ID of file associated with the remove event.
     */
    const std::string &fileUuid() const;

    /**
     * Aggregates @c this event with an other event.
     * Aggregation is done by addition of events' counters
     * @param event RemoveFileEvent to be aggregated.
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
