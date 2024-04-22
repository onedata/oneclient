/**
 * @file router.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_ROUTER_H
#define ONECLIENT_EVENTS_ROUTER_H

#include "events/declarations.h"

namespace one {
namespace client {
namespace events {

/**
 * @c Router is reponsible for handling, deserialization and forwarding
 * event-specific messages to the event manager.
 */
class Router {
    friend class Manager;

public:
    /**
     * Constructor.
     * @param A @c Manager instance.
     * @param A @c communication::Communicator instance.
     */
    Router(Manager &manager, communication::Communicator &communicator);

private:
    // Handle remote events received from Oneprovider
    void handle(const ProtoEvents &msg);
    // Handle remote subscriptions (from Oneprovider to Oneclient), currently
    // this includes only FileWritten and FileRead
    void handle(const ProtoSubscription &msg);
    // Handle remote subscription cancellation
    void handle(const ProtoCancellation &msg);

    Manager &m_eventManager;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_ROUTER_H
