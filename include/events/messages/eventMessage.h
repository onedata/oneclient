/**
* @file eventMessage.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_EVENT_MESSAGE_H
#define ONECLIENT_EVENTS_MESSAGES_EVENT_MESSAGE_H

#include <memory>

namespace one {

namespace clproto {
namespace communication_protocol {
class Answer;
}
}

namespace client {
namespace events {

class EventManager;

class EventMessage {
public:
    virtual ~EventMessage() = default;

    virtual bool process(EventManager &manager) const = 0;
};

class EventMessageSerializer {
public:
    using Message = one::clproto::communication_protocol::Answer;

    virtual ~EventMessageSerializer() = default;

    virtual std::unique_ptr<EventMessage>
    deserialize(const Message &message) const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif