/**
 * @file ioEventStream.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_STREAMS_IO_EVENT_STREAM_H
#define ONECLIENT_EVENTS_STREAMS_IO_EVENT_STREAM_H

#include "context.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"
#include "events/subscriptions/ioEventSubscription.h"
#include "logging.h"
#include "scheduler.h"

#include <asio/io_service_strand.hpp>

#include <sys/types.h>

#include <string>
#include <memory>
#include <cstddef>
#include <utility>
#include <functional>

namespace one {
namespace client {
namespace events {

template <class EventT> class EventBuffer;
class EventCommunicator;

/**
 * The IOEventStream class is responsible aggregation and emission of IO events.
 */
template <class EventT> class IOEventStream {
public:
    /**
     * Constructor.
     * @param context A @c Context instance used to acquire @c Scheduler
     * instance which is later used to schedule periodic events emission from
     * the stream.
     * @param communicator An @c EventCommunicator instance to which emitted
     * event are forwarded.
     */
    IOEventStream(std::weak_ptr<Context> ctx, const EventCommunicator &evtComm);

    virtual ~IOEventStream() = default;

    const asio::io_service::strand &strand() const;

    /**
     * Pushes an event to the stream.
     * @param evt An event to be pushed to the stream.
     */
    virtual void push(EventT evt);

    /**
     * Adds a subscription for events.
     * @param evtSub A subscription to be added.
     * @return ID of subscription and subscription cancellation function.
     */
    virtual std::pair<uint64_t, std::function<void()>> subscribe(
        IOEventSubscription<EventT> evtSub);

    /**
     * Removes a subscription for events.
     * @param evtSub A subscription to be removed.
     */
    virtual void unsubscribe(IOEventSubscription<EventT> evtSub);

private:
    void periodicEmission();
    void resetPeriodicEmission();

    std::weak_ptr<Context> m_ctx;
    const EventCommunicator &m_evtComm;
    IOEventSubscription<EventT> m_evtSub;
    std::unique_ptr<EventBuffer<EventT>> m_evtBuff;

    asio::io_service::strand m_strand;
    std::function<void()> m_cancelPeriodicEmission = [] {};
};

template <class EventT>
IOEventStream<EventT>::IOEventStream(
    std::weak_ptr<Context> ctx, const EventCommunicator &evtComm)
    : m_ctx{std::move(ctx)}
    , m_evtComm{evtComm}
    , m_evtBuff{new VoidEventBuffer<EventT>{}}
    , m_strand{m_ctx.lock()->scheduler()->getIoService()}
{
}

template <class EventT>
const asio::io_service::strand &IOEventStream<EventT>::strand() const
{
    return m_strand;
}

template <class EventT> void IOEventStream<EventT>::push(EventT evt)
{
    if (m_evtBuff->push(std::move(evt)))
        resetPeriodicEmission();
}

template <class EventT>
std::pair<uint64_t, std::function<void()>> IOEventStream<EventT>::subscribe(
    IOEventSubscription<EventT> evtSub)
{
    LOG(INFO) << "Adding subscription: " << evtSub.toString();

    auto id = evtSub.id();

    if (m_evtSub.empty() && !evtSub.empty())
        m_evtBuff.reset(new IOEventBuffer<EventT>(m_evtComm, m_evtSub));
    m_evtSub += evtSub;
    if (!m_evtSub.timeThresholds().empty() &&
        evtSub.timeThresholds() <= m_evtSub.timeThresholds()) {
        m_evtBuff->clear();
        resetPeriodicEmission();
    }
    else if (m_evtBuff->try_clear())
        resetPeriodicEmission();

    return {id, [ this, evtSub = std::move(evtSub) ]
    {
        m_ctx.lock()->scheduler()->post(
            m_strand, [ this, evtSub = std::move(evtSub) ] {
                unsubscribe(std::move(evtSub));
            });
    }};
}

template <class EventT>
void IOEventStream<EventT>::unsubscribe(IOEventSubscription<EventT> evtSub)
{
    LOG(INFO) << "Removing subscription: " << evtSub.toString();

    m_evtSub -= evtSub;
    if (m_evtSub.empty() && !evtSub.empty()) {
        m_evtBuff.reset(new VoidEventBuffer<EventT>());
        resetPeriodicEmission();
    }
}

template <class EventT> void IOEventStream<EventT>::periodicEmission()
{
    m_ctx.lock()->scheduler()->post(m_strand, [this] {
        m_evtBuff->clear();
        resetPeriodicEmission();
    });
}

template <class EventT> void IOEventStream<EventT>::resetPeriodicEmission()
{
    m_cancelPeriodicEmission();
    if (!m_evtSub.timeThresholds().empty())
        m_cancelPeriodicEmission = m_ctx.lock()->scheduler()->schedule(
            *m_evtSub.timeThresholds().begin(),
            std::bind(&IOEventStream<EventT>::periodicEmission, this));
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_STREAMS_IO_EVENT_STREAM_H
