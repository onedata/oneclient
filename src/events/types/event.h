/**
 * @file event.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_EVENT_H
#define ONECLIENT_EVENTS_TYPES_EVENT_H

#include <cstddef>
#include <memory>

namespace one {
namespace clproto {
class Event;
} // namespace clproto
namespace client {
namespace events {

using ProtocolEventMessage = one::clproto::Event;

/**
 * @c Event class represents an event that occured in the system.
 */
class Event {
public:
    virtual ~Event() = default;

    /**
     * @return Number of aggregated events.
     */
    std::size_t counter() const { return m_counter; }

    /**
     * @return @c Event in string format.
     */
    virtual std::string toString() const = 0;

    /**
     * Creates Protocol Buffers message based on provided @c Event.
     * @return Unique pointer to Protocol Buffers @c Event message instance.
     */
    virtual std::unique_ptr<ProtocolEventMessage> serializeAndDestroy() = 0;

protected:
    std::size_t m_counter = 1;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_EVENT_H
