/**
 * @file voidEventBuffer.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_BUFFERS_VOID_BUFFER_H
#define ONECLIENT_EVENTS_BUFFERS_VOID_BUFFER_H

#include "eventBuffer.h"

namespace one {
namespace client {
namespace events {

/**
 * The VoidEventBuffer class is an implementation of Null Object design pattern.
 * The buffer ignores provided events.
 */
template <class EventT> class VoidEventBuffer : public EventBuffer<EventT> {
public:
    /**
     * @copydoc EventBuffer::push(EventT evt)
     * @c VoidEventBuffer ignores provided @p event.
     */
    bool push(EventT) override { return false; }

    /**
     * @copydoc EventBuffer::clear()
     */
    void clear() override {}

    /**
     * @copydoc EventBuffer::try_clear()
     */
    bool try_clear() override { return false; }
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_BUFFERS_VOID_BUFFER_H
