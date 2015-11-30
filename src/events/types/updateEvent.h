/**
 * @file updateEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_UPDATE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_UPDATE_EVENT_H

#include "event.h"
#include "messages/fuse/fileAttr.h"

#include <memory>
#include <sstream>

namespace one {
namespace client {
namespace events {

/**
 * @c UpdateEvent class represents an update operation that occured in the
 * file system.
 */
template <class Wrapped> class UpdateEvent : public Event {
public:
    using EventPtr = std::unique_ptr<UpdateEvent<Wrapped>>;
    using Key = typename Wrapped::Key;
    using ProtocolMessage = typename Wrapped::ProtocolMessage;
    using Subscription = typename Wrapped::Subscription;

    /**
     * Constructor.
     * @param message Protocol Buffers message representing @c Wrapped
     * counterpart.
     */
    UpdateEvent(const ProtocolMessage &message)
        : m_wrapped{std::make_unique<Wrapped>(message)}
    {
    }

    /**
     * @return Value that distinguish @c this update event from other update
     * events, i.e. update events with the same key can be aggregated.
     * @see @c UpdateEvent::Key.
     */
    const Key &key() const;

    /**
     * Aggregates @c this event with an other update event.
     * Aggregation is done by addition of events' counters.
     * @param event Update event to be aggregated.
     */
    void aggregate(EventPtr event);

    std::string toString() const override;

    std::unique_ptr<ProtocolEventMessage> serialize() const override;

private:
    std::unique_ptr<Wrapped> m_wrapped;
};

template <class Wrapped>
const typename UpdateEvent<Wrapped>::Key &UpdateEvent<Wrapped>::key() const
{
    return m_wrapped->key();
}

template <class Wrapped> void UpdateEvent<Wrapped>::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
    m_wrapped->aggregate(std::move(event->m_wrapped));
}

template <class Wrapped> std::string UpdateEvent<Wrapped>::toString() const
{
    std::stringstream stream;
    stream << "type: 'UpdateEvent', counter: " << m_counter
           << ", wrapped: " << m_wrapped->toString();
    return stream.str();
}

template <class Wrapped>
std::unique_ptr<ProtocolEventMessage> UpdateEvent<Wrapped>::serialize() const
{
    return {};
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_UPDATE_EVENT_H
