/**
* @file eventCommunicator.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H
#define ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H

#include <memory>

namespace one {
namespace client {

class Context;

namespace events {

class Event;

/**
* The EventCommunicator class is responsible for sending event messages to the
* server using communication stream.
*/
class EventCommunicator {
public:
    /**
    * Constructor.
    * @param context A @c Context instance used to acquire communication stream.
    */
    EventCommunicator(std::weak_ptr<Context> context);

    virtual ~EventCommunicator() = default;

    /**
    * Sends event to the server using communication stream.
    * @param event Event to be sent.
    */
    virtual void send(const Event &event) const;

private:
    std::weak_ptr<Context> m_context;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H