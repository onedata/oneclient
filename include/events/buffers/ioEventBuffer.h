/**
 * @file ioEventBuffer.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_BUFFERS_IO_EVENT_BUFFER_H
#define ONECLIENT_EVENTS_BUFFERS_IO_EVENT_BUFFER_H

#include "eventBuffer.h"
#include "events/eventCommunicator.h"
#include "events/subscriptions/ioEventSubscription.h"

#include <vector>
#include <algorithm>
#include <functional>
#include <unordered_map>

namespace one {
namespace client {
namespace events {

class EventCommunicator;

/**
 * The IOEventBuffer class represents an aggregator that aggregates IO events.
 */
template <class EventT> class IOEventBuffer : public EventBuffer<EventT> {
public:
    IOEventBuffer(const EventCommunicator &evtComm,
        const IOEventSubscription<EventT> &evtSub);

    virtual ~IOEventBuffer();

    /**
     * @copydoc EventBuffer::push(EventT evt)
     * @c IOEventBuffer aggregates IO events with the same file ID.
     */
    bool push(EventT evt) override;

    /**
     * @copydoc EventBuffer::clear()
     * Forwards stored events to the @c EventCommunicator.
     */
    void clear() override;

    /**
     * @copydoc clear()
     * Checks whether subscription is satisfied before events emission.
     */
    bool try_clear() override;

private:
    const EventCommunicator &m_evtComm;
    const IOEventSubscription<EventT> &m_evtSub;
    std::size_t m_sumCtr = 0;
    std::size_t m_sumSize = 0;
    std::unordered_map<std::string, EventT> m_evtByFileId;
};

template <class EventT>
IOEventBuffer<EventT>::IOEventBuffer(
    const EventCommunicator &evtComm, const IOEventSubscription<EventT> &evtSub)
    : m_evtComm{evtComm}
    , m_evtSub{evtSub}
{
}

template <class EventT> IOEventBuffer<EventT>::~IOEventBuffer() { clear(); }

template <class EventT> bool IOEventBuffer<EventT>::push(EventT evt)
{
    m_sumCtr += evt.counter();
    m_sumSize += evt.size();

    auto found = m_evtByFileId.find(evt.fileUuid());
    if (found != m_evtByFileId.end())
        found->second += evt;
    else
        m_evtByFileId.emplace(evt.fileUuid(), std::move(evt));

    return try_clear();
}

template <class EventT> void IOEventBuffer<EventT>::clear()
{
    for (const auto &evtByFileId : m_evtByFileId)
        m_evtComm.send(evtByFileId.second);

    m_sumCtr = 0;
    m_sumSize = 0;
    m_evtByFileId.clear();
}

template <class EventT> bool IOEventBuffer<EventT>::try_clear()
{
    if (m_evtSub.satisfied(m_sumCtr, m_sumSize)) {
        clear();
        return true;
    }
    return false;
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_BUFFERS_IO_EVENT_BUFFER_H
