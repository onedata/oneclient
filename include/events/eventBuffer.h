/**
* @file eventBuffer.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_EVENT_BUFFER_H
#define ONECLIENT_EVENTS_EVENT_BUFFER_H

#include <google/protobuf/message.h>

#include <queue>
#include <deque>
#include <memory>
#include <mutex>
#include <thread>
#include <condition_variable>

namespace one {
namespace client {
namespace events {

class Event;
class EventCommunicator;

class EventBuffer {
public:
    EventBuffer(std::weak_ptr<EventCommunicator> communicator);

    ~EventBuffer();

    void push(std::unique_ptr<Event> event);

    const google::protobuf::Message &getSentMessage(unsigned long long id);

    void removeSentMessages(unsigned long long id);

private:
    void processPendingEvents();

    void push(std::unique_ptr<google::protobuf::Message> message);

    std::weak_ptr<EventCommunicator> m_communicator;
    bool m_isThreadRunning;
    std::thread m_thread;
    std::condition_variable m_pendingEventsConditionVariable;
    std::mutex m_pendingEventsMutex;
    std::mutex m_sentMessagesMutex;
    unsigned long long m_sequenceNumber;
    unsigned long long m_lastConfirmedMessageId;
    std::queue<std::unique_ptr<Event>> m_pendingEvents;
    std::deque<std::unique_ptr<google::protobuf::Message>> m_sentMessages;
};

} // namespace events
} // namespace client
} // namespace one

#endif