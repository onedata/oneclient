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

class RemoteEvent : public Event {
public:
    ~RemoteEvent() = default;

    virtual ProtoEventPtr serializeAndDestroy() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_REMOTE_EVENT_H
