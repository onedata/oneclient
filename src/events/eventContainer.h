/**
 * @file eventContainer.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EVENT_CONTAINER_H
#define ONECLIENT_EVENTS_EVENT_CONTAINER_H

#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "messages/clientMessage.h"

#include <memory>
#include <sstream>
#include <vector>

namespace one {
namespace client {
namespace events {

/**
 * The @c EventContainer class represents container for events of the same type.
 * It is used to send multiple aggregated events as one message.
 */
template <class EventT> class EventContainer : public messages::ClientMessage {
public:
    using EventPtr = typename EventT::EventPtr;

    /**
     * Constructor.
     * @param events Events to send.
     */
    EventContainer(std::vector<EventPtr> events);

    std::string toString() const override;

private:
    std::unique_ptr<messages::ProtocolClientMessage>
    serializeAndDestroy() override;

    std::vector<EventPtr> m_events;
};

template <class EventT>
EventContainer<EventT>::EventContainer(std::vector<EventPtr> events)
    : m_events{std::move(events)}
{
}

template <class EventT>
std::unique_ptr<messages::ProtocolClientMessage>
EventContainer<EventT>::serializeAndDestroy()
{
    auto clientMsg = std::make_unique<messages::ProtocolClientMessage>();
    auto eventsMsg = clientMsg->mutable_events();

    for (auto &event : m_events) {
        auto eventMsg = eventsMsg->add_events();
        eventMsg->Swap(event->serializeAndDestroy().release());
    }

    return clientMsg;
}

template <class EventT> std::string EventContainer<EventT>::toString() const
{
    std::stringstream stream;
    stream << "type: 'EventContainer', events: [ ";
    for (const auto &event : m_events)
        stream << event->toString() << ", ";
    stream << " ]";
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EVENT_CONTAINER_H
