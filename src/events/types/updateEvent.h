/**
 * @file updateEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_UPDATE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_UPDATE_EVENT_H

#include "event.h"

namespace one {
namespace client {
namespace events {

template <class Wrapped> class UpdateEvent : public Event {
    using ProtocolMessage = typename Wrapped::ProtocolMessage;

public:
    UpdateEvent(const ProtocolMessage &msg)
        : m_wrapped{std::make_shared<Wrapped>(msg)}
    {
    }

    UpdateEvent(std::shared_ptr<Wrapped> wrapped)
        : m_wrapped{std::move(wrapped)}
    {
    }

    const std::string &routingKey() const override
    {
        return m_wrapped->routingKey();
    }

    const std::string &aggregationKey() const override
    {
        return m_wrapped->aggregationKey();
    }

    const Wrapped &wrapped() const { return *m_wrapped; }

    std::string toString() const override
    {
        std::stringstream stream;
        stream << "type: 'Update', wrapped: " << m_wrapped->toString();
        return stream.str();
    }

    void aggregate(ConstEventPtr event) override
    {
        auto updateEvent = events::get<UpdateEvent>(event);
        m_wrapped->aggregate(updateEvent->m_wrapped);
    }

    EventPtr clone() const override
    {
        return std::make_shared<UpdateEvent>(
            std::static_pointer_cast<Wrapped>(m_wrapped->clone()));
    }

private:
    std::shared_ptr<Wrapped> m_wrapped;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_UPDATE_EVENT_H
