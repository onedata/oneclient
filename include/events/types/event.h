/**
* @file event.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_TYPES_EVENT_H
#define ONECLIENT_EVENTS_TYPES_EVENT_H

#include <google/protobuf/message.h>

#include <memory>

namespace one {
namespace client {
namespace events {

class EventSerializer;

/**
* The Event abstract class provides an interface for all event types.
*/
class Event {
public:
    virtual ~Event() = default;

    /**
    * Emits an event.
    */
    virtual void emit() = 0;

    /**
    * Returns an @EventSerializer instance for an event.
    * @return An @c EventSerializer instance.
    */
    virtual std::unique_ptr<EventSerializer> serializer() const = 0;

protected:
    size_t m_counter = 1;
};

/**
* The EventSerializer abstract class provides an interface for all event
* serializers.
*/
class EventSerializer {
public:
    virtual ~EventSerializer() = default;

    /**
    * Serializes an event to Protocol Buffers message.
    * @param sequenceNumber Unique number associated with each message sent by
    * an @c EventCommunicator.
    * @param event An @c Event instance to be serialized.
    * @return A serialized event message.
    */
    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber, const Event &event) const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_EVENT_H