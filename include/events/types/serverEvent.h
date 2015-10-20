/**
 * @file serverEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_SERVER_EVENT_H
#define ONECLIENT_EVENTS_TYPES_SERVER_EVENT_H

#include "event.h"
#include "messages/serverMessage.h"

namespace one {
namespace client {
namespace events {

/**
 * @c ServerEvent class represents an event that occured on the server side.
 */
class ServerEvent : public Event, public messages::ServerMessage {
public:
    virtual ~ServerEvent() = default;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_SERVER_EVENT_H
