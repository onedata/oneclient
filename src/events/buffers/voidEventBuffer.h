/**
 * @file voidEventBuffer.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_BUFFER_VOID_EVENT_BUFFER_H
#define ONECLIENT_EVENTS_BUFFER_VOID_EVENT_BUFFER_H

#include "eventBuffer.h"

namespace one {
namespace client {
namespace events {

/**
 * @c VoidEventBuffer class is an implementation of Null Object design pattern.
 * The buffer ignores provided events.
 */
template <class EventT> class VoidEventBuffer : public EventBuffer<EventT> {
public:
    using EventPtr = typename EventBuffer<EventT>::EventPtr;
    using EventHandler = typename EventBuffer<EventT>::EventHandler;

    /**
     * @copydoc EventBuffer::push(EventPtr)
     * @c VoidEventBuffer ignores provided @p event.
     */
    void push(EventPtr) override;

    /**
      * @copydoc EventBuffer::clear()
      */
    void clear() override;

    /**
      * @copydoc EventBuffer::setOnClearHandler(EventHandler)
      */
    void setOnClearHandler(EventHandler) override;
};

template <class EventT> void VoidEventBuffer<EventT>::push(EventPtr) {}

template <class EventT> void VoidEventBuffer<EventT>::clear() {}

template <class EventT>
void VoidEventBuffer<EventT>::setOnClearHandler(EventHandler)
{
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_BUFFER_VOID_EVENT_BUFFER_H
