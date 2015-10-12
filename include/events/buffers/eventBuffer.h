/**
 * @file eventBuffer.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_H
#define ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_H

namespace one {
namespace client {
namespace events {

/**
 * The EventBuffer abstract class provides an interface for all buffers.
 */
template <class EventT> class EventBuffer {
public:
    virtual ~EventBuffer() = default;

    /**
     * Adds an event to the buffer.
     * @param evt Event to be added to the buffer. It should be aggregable
     * using @c += operator.
     * @return 'true' if buffer was cleared after addition, otherwise 'false'.
     */
    virtual bool push(EventT evt) = 0;

    /**
     * Clears the buffer.
     */
    virtual void clear() = 0;

    /**
     * Tries to clears the buffer.
     * @return 'true' if buffer was cleared, otherwise 'false'.
     */
    virtual bool try_clear() = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_H
