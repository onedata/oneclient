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

class Event {
public:
    virtual ~Event() = default;

    virtual void emit() = 0;

    virtual std::unique_ptr<EventSerializer> serializer() const = 0;

protected:
    size_t m_counter = 1;
};

class EventSerializer {
public:
    virtual ~EventSerializer() = default;

    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber, const Event &event) const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif