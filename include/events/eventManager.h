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
class EventFactory;
class ReadEventStream;
class WriteEventStream;
class EventCommunicator;

class EventManager {
    using Message = one::clproto::communication_protocol::Answer;

public:
    EventManager(std::shared_ptr<Context> context);

    bool handle(const Message &message);

    bool cancelSubscription(unsigned long long id);

    std::unique_ptr<Event> createReadEvent(const std::string &fileId,
                                           off_t offset, size_t size) const;

    std::unique_ptr<Event> createWriteEvent(const std::string &fileId,
                                            off_t offset, size_t size,
                                            off_t fileSize) const;

    std::unique_ptr<Event> createTruncateEvent(const std::string &fileId,
                                               off_t fileSize) const;

private:
    std::shared_ptr<Context> m_context;
    std::shared_ptr<EventFactory> m_factory;
    std::shared_ptr<EventCommunicator> m_communicator;
    std::shared_ptr<EventBuffer> m_buffer;
    std::shared_ptr<ReadEventStream> m_readEventStream;
    std::shared_ptr<WriteEventStream> m_writeEventStream;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_MANAGER_H