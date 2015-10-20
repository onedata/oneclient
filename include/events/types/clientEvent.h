/**
 * @file clientEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_CLIENT_EVENT_H
#define ONECLIENT_EVENTS_TYPES_CLIENT_EVENT_H

#include "event.h"
#include "messages/clientMessage.h"

namespace one {
namespace client {
namespace events {

/**
 * @c ClientEvent class represents an event that occured on the client side.
 */
class ClientEvent : public Event, public messages::ClientMessage {
public:
    virtual ~ClientEvent() = default;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_CLIENT_EVENT_H
