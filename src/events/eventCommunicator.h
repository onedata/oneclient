/**
 * @file eventCommunicator.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H
#define ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H

#include "communication/communicator.h"
#include "events/eventContainer.h"
#include "events/buffers/voidEventBuffer.h"
#include "events/subscriptions/subscriptionCancellation.h"

#include <functional>
#include <unordered_map>
#include <vector>

namespace one {
namespace client {
namespace events {

/**
 * @c EventCommunicator is responsible for sending events and subscriptions to
 * the server. It is a wrapper on @c one::communication::StreamManager::Stream.
 */
template <class EventType> class EventCommunicator {
public:
    using EventT = EventType;
    using EventPtr = typename EventT::EventPtr;
    using Subscription = typename EventT::Subscription;
    using SubscriptionPtr = std::unique_ptr<Subscription>;
    using StreamPtr = std::shared_ptr<communication::StreamManager::Stream>;

    /**
     * A reference to @c *this typed as a @c EventCommunicator.
     */
    EventCommunicator<EventT> &communicator = *this;

    /**
     * Constructor.
     * @param stream Communication stream.
     */
    EventCommunicator(StreamPtr stream);

    /**
     * Destructor.
     * Closes the underlying communication stream.
     */
    virtual ~EventCommunicator();

    /**
     * Sends events to the server.
     * @param events Events to be sent.
     */
    void send(std::vector<EventPtr> events);

    /**
     * Sends a subscription to the server.
     * @param subscription Subscription to be sent.
     */
    void send(const Subscription &subscription);

    /**
     * Sends a subscription cancellaion to the server.
     * @param cancellaion Subscription cancellaion to be sent.
     */
    void send(const SubscriptionCancellation &cancellation);

private:
    StreamPtr m_stream;
};

template <class EventT>
EventCommunicator<EventT>::EventCommunicator(StreamPtr stream)
    : m_stream{std::move(stream)}
{
}

template <class EventT> EventCommunicator<EventT>::~EventCommunicator()
{
    m_stream->close();
}

template <class EventT>
void EventCommunicator<EventT>::send(std::vector<EventPtr> events)
{
    if (!events.empty()) {
        EventContainer<EventT> eventsMsg{std::move(events)};
        m_stream->send(eventsMsg);
    }
}

template <class EventT>
void EventCommunicator<EventT>::send(const Subscription &subscription)
{
    m_stream->send(subscription);
}

template <class EventT>
void EventCommunicator<EventT>::send(
    const SubscriptionCancellation &cancellation)
{
    m_stream->send(cancellation);
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_COMMUNICATOR_H
