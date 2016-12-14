/**
 * @file remoteEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_REMOTE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_REMOTE_EVENT_H

#include "event.h"

namespace one {
namespace client {
namespace events {

/**
 * @c RemoteEvent class represents an abstract event that should be sent to the
 * remote subscriber. It provides an interface for concrete remote events.
 */
class RemoteEvent : public Event {
public:
    ~RemoteEvent() = default;

    /**
     * Creates Protocol Buffers message based on provided @c RemoteEvent.
     * The instance of RemoteEvent is invalidated after its use.
     * @return A serialized remote event.
     */
    virtual ProtoEventPtr serializeAndDestroy() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_REMOTE_EVENT_H
