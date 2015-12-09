/**
 * @file eventBuffer.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_H
#define ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_H

#include <memory>
#include <vector>

namespace one {
namespace client {
namespace events {

/**
 * @c EventBuffer is an abstract class that provides an interface for all event
 * buffers.
 */
template <class EventT> class EventBuffer {
public:
    using EventPtr = typename EventT::EventPtr;
    using EventHandler = std::function<void(std::vector<EventPtr>)>;

    virtual ~EventBuffer() = default;

    /**
     * Adds an event to the buffer.
     * @param event Event to be added.
     */
    virtual void push(EventPtr event) = 0;

    /**
     * Removes all events from the buffer.
     */
    virtual void clear() = 0;

    /**
     * Set a handler which is called during @c clear method execution.
     * @param handler Handler to be set.
     */
    virtual void setOnClearHandler(EventHandler handler) = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_H
