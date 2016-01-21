/**
 * @file permissionChangedEvent.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_PERMISSION_CHANGED_EVENT_H
#define ONECLIENT_EVENTS_TYPES_PERMISSION_CHANGED_EVENT_H

#include "event.h"
#include "messages/serverMessage.h"

#include <boost/icl/interval_map.hpp>

#include <sys/types.h>

#include <string>
#include <memory>

namespace one {
namespace client {
namespace events {

class PermissionChangedSubscription;

/**
 * @c PermissionChangedEvent class represents an event of changing file mode
 */
class PermissionChangedEvent : public Event {
public:
    using EventPtr = std::unique_ptr<PermissionChangedEvent>;
    using Key = std::string;
    using Subscription = PermissionChangedSubscription;

    /**
     * Constructor.
     * @param fileUuid UUID of a file associated with an event.
     */
    PermissionChangedEvent(std::string fileUuid);

    /**
     * @return Value that distinguish @c this read event from other read events,
     * i.e. read events with the same key can be aggregated.
     * @see @c PermissionChangedEvent::Key.
     */
    const Key &key() const;

    /**
     * @return ID of file associated with the read event.
     */
    const std::string &fileUuid() const;

    /**
     * Aggregates @c this event with an other event.
     * Aggregation is done by addition of events' counters
     * @param event PermissionChangedEvent to be aggregated.
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

#endif // ONECLIENT_EVENTS_TYPES_PERMISSION_CHANGED_EVENT_H
