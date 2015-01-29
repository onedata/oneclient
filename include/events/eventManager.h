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

/**
* The EventManager class is responsible for events management. It handles and
* dispatch client requests.
*/
class EventManager {
    using Message = one::clproto::communication_protocol::Answer;

public:
    /**
    * Constructor.
    * context A @c Context instance.
    */
    EventManager(std::shared_ptr<Context> context);

    /**
    * A @c PushListener callback. @see PushListener::subscribe
    * Handles and dispatches client requests.
    * @message Message sent by client.
    * @return Allways returns @c true.
    */
    bool handle(const Message &message);

    /**
    * Cancels subscription by subscription ID.
    * @param id ID of subscription to be cancelled.
    * @return Returns @c true in case of successful subscription cancellation or
    * @c false otherwise.
    */
    bool cancelSubscription(unsigned long long id) const;

    /**
    * Delegates creation of read event instance to an @c EventFactory.
    * @param fileId ID of file associated with a read operation.
    * @param offset Distance from the beginning of the file to the first byte
    * read.
    * @param size Amount of bytes read.
    * @return Returns a @c ReadEvent instance.
    */
    std::unique_ptr<Event> createReadEvent(const std::string &fileId,
                                           off_t offset, size_t size) const;

    /**
    * Delegates creation of write event instance to an @c EventFactory.
    * @param fileId ID of file associated with a read operation.
    * @param offset Distance from the beginning of the file to the first byte
    * read.
    * @param size Amount of bytes read.
    * @param fileSize Size of file associated with a write operation.
    * @return Returns a @c WriteEvent instance.
    */
    std::unique_ptr<Event> createWriteEvent(const std::string &fileId,
                                            off_t offset, size_t size,
                                            off_t fileSize) const;

    /**
    * Delegates creation of truncate event instance to an @c EventFactory.
    * @param fileId ID of file associated with a read operation.
    * @param fileSize Size of file associated with a write operation.
    * @return Returns a @c WriteEvent instance.
    */
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