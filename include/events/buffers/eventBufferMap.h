/**
 * @file eventBufferMap.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_MAP_H
#define ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_MAP_H

#include "eventBuffer.h"

#include <functional>
#include <unordered_map>
#include <vector>

namespace one {
namespace client {
namespace events {

/**
 * @c EventBufferMap is responsible for storing events and aggregating the by
 * key.
 */
template <class EventT> class EventBufferMap : public EventBuffer<EventT> {
public:
    using EventPtr = typename EventBuffer<EventT>::EventPtr;
    using EventHandler = typename EventBuffer<EventT>::EventHandler;

    /**
     * Destructor.
     * Removes all events from the buffer by calling @c clear method.
     */
    ~EventBufferMap();

    /**
     * @copydoc EventBuffer::push(EventPtr)
     * Aggregates events with the same key (@see e.g. @c ReadEvent::Key).
     */
    void push(EventPtr event) override;

    /**
     * @copydoc EventBuffer::clear()
     * Calls @c EventHandler on each removed event.
     */
    void clear() override;

    /**
     * @copydoc EventBuffer::setOnClearHandler(EventHandler)
     */
    void setOnClearHandler(EventHandler handler) override;

private:
    EventHandler m_onClearHandler = [](auto) {};
    std::unordered_map<typename EventT::Key, EventPtr> m_buffer;
};

template <class EventT> EventBufferMap<EventT>::~EventBufferMap() { clear(); }

template <class EventT> void EventBufferMap<EventT>::push(EventPtr event)
{
    auto it = m_buffer.find(event->key());
    if (it != m_buffer.end())
        it->second->aggregate(std::move(event));
    else {
        auto key = event->key();
        m_buffer.emplace(key, std::move(event));
    }
}

template <class EventT> void EventBufferMap<EventT>::clear()
{
    std::vector<EventPtr> events;
    for (auto &entry : m_buffer)
        events.emplace_back(std::move(entry.second));
    m_onClearHandler(std::move(events));
    m_buffer.clear();
}

template <class EventT>
void EventBufferMap<EventT>::setOnClearHandler(EventHandler handler)
{
    m_onClearHandler = std::move(handler);
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_BUFFERS_EVENT_BUFFER_MAP_H
