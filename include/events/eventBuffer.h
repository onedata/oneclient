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

    const google::protobuf::Message &
    getSentMessage(unsigned long long sequenceNumber);

    void removeSentMessages(unsigned long long sequenceNumber);

private:
    void processPendingEvents();

    void push(std::unique_ptr<google::protobuf::Message> message);

    std::weak_ptr<EventCommunicator> m_communicator;
    bool m_isThreadRunning = true;
    std::thread m_thread;
    std::condition_variable m_pendingEventsConditionVariable;
    std::mutex m_pendingEventsMutex;
    std::mutex m_sentMessagesMutex;
    unsigned long long m_sequenceNumber = 1;
    unsigned long long m_lastConfirmedSequenceNumber = 0;
    std::queue<std::unique_ptr<Event>> m_pendingEvents;
    std::deque<std::unique_ptr<google::protobuf::Message>> m_sentMessages;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_BUFFER_H