/**
* @file eventBuffer.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "logging.h"

#include "events/types/event.h"
#include "events/eventBuffer.h"
#include "events/eventCommunicator.h"

namespace one {
namespace client {
namespace events {

EventBuffer::EventBuffer(std::weak_ptr<EventCommunicator> communicator)
    : m_communicator{std::move(communicator)}
{
    m_thread = std::thread(&EventBuffer::processPendingEvents, this);
}

EventBuffer::~EventBuffer()
{
    m_isThreadRunning = false;
    m_pendingEventsConditionVariable.notify_one();
    m_thread.join();
}

void EventBuffer::push(std::unique_ptr<Event> event)
{
    std::lock_guard<std::mutex> guard{m_pendingEventsMutex};
    m_pendingEvents.push(std::move(event));
    m_pendingEventsConditionVariable.notify_one();
}

const google::protobuf::Message &
EventBuffer::getSentMessage(unsigned long long sequenceNumber)
{
    std::lock_guard<std::mutex> guard{m_sentMessagesMutex};
    if (sequenceNumber <=
        m_sentMessages.size() - m_lastConfirmedSequenceNumber) {
        return *m_sentMessages[sequenceNumber - m_lastConfirmedSequenceNumber -
                               1];
    } else {
        throw std::out_of_range{"Message not found."};
    }
}

void EventBuffer::removeSentMessages(unsigned long long sequenceNumber)
{
    std::lock_guard<std::mutex> guard{m_sentMessagesMutex};
    while (!m_sentMessages.empty() &&
           m_lastConfirmedSequenceNumber <= sequenceNumber) {
        m_sentMessages.pop_front();
        ++m_lastConfirmedSequenceNumber;
    }
}

void EventBuffer::processPendingEvents()
{
    while (m_isThreadRunning) {
        std::unique_lock<std::mutex> lock(m_pendingEventsMutex);
        if (m_pendingEvents.empty())
            m_pendingEventsConditionVariable.wait(lock);
        if (m_pendingEvents.empty())
            continue;
        auto event = std::move(m_pendingEvents.front());
        auto message = event->serializer()->serialize(m_sequenceNumber, *event);
        ++m_sequenceNumber;
        if (m_communicator.lock()->send(*message))
            push(std::move(message));
        m_pendingEvents.pop();
    }
}

void EventBuffer::push(std::unique_ptr<google::protobuf::Message> message)
{
    std::lock_guard<std::mutex> guard{m_sentMessagesMutex};
    m_sentMessages.push_back(std::move(message));
}

} // namespace events
} // namespace client
} // namespace one
