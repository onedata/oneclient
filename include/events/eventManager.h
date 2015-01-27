/**
* @file eventManager.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_MANAGER_H
#define ONECLIENT_EVENTS_EVENT_MANAGER_H

#include <memory>

namespace one {

namespace clproto {
namespace communication_protocol {
class Answer;
}
}

namespace client {

class Context;

namespace events {

class Event;
class EventBuffer;
class EventMapper;
class EventFactory;
class EventCommunicator;
class EventSubscription;
class EventMessageSerializer;

class EventManager {
    using Message = one::clproto::communication_protocol::Answer;

public:
    EventManager(std::shared_ptr<Context> context);

    const EventFactory &eventFactory() const;

    void emit(unsigned long long id);

    void emit(const Event &event);

    void removeConfirmedEvents(unsigned long long id);

    bool handle(const Message &message);

    const std::string &subscribe(const EventSubscription &subscription);

    bool unsubscribe(const std::string &id);

private:
    std::shared_ptr<Context> m_context;
    std::shared_ptr<EventFactory> m_factory;
    std::shared_ptr<EventCommunicator> m_communicator;
    std::shared_ptr<EventBuffer> m_buffer;
    std::shared_ptr<EventMapper> m_mapper;
};

} // namespace events
} // namespace client
} // namespace one

#endif