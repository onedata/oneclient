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

/**
* The EventBuffer class is responsible for buffering events to be sent and
* storing unconfirmed events.
*/
class EventBuffer {
public:
    /**
    * Constructor.
    * @param communicator An @EventCommunicator instance.
    */
    EventBuffer(std::weak_ptr<EventCommunicator> communicator);

    ~EventBuffer();

    /**
    * Pushed an event to the buffer.
    * @param event Event to be pushed to the buffer.
    */
    void push(std::unique_ptr<Event> event);

    /**
    * Returns unconfirmed event by sequence number or throws an exception if an
    * event associated with given @p sequenceNumber is missing.
    * @param sequenceNumber Sequence number of an event.
    * @return Serialized event message or throws an exception if event is
    * missing.
    */
    const google::protobuf::Message &
    getSentMessage(unsigned long long sequenceNumber);

    /**
    * Removes all stored event messages up to given @p sequenceNumber.
    * @param sequenceNumber Sequence number of last event message to be removed.
    */
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