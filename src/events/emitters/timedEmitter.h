/**
 * @file timedEmitter.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_EMITTERS_TIMED_EMITTER_H
#define ONECLIENT_EVENTS_EMITTERS_TIMED_EMITTER_H

#include "emitter.h"
#include "events/manager.h"
#include "events/streams/stream.h"
#include "falseEmitter.h"

#include <chrono>
#include <functional>
#include <string>

namespace one {
namespace client {
namespace events {

/**
 * @c TimedEmitter is responsible for periodic event stream flush.
 */
template <class T, class Scheduler = Scheduler>
class TimedEmitter : public Emitter<T> {
public:
    /**
     * Constructor.
     * @param streamKey A key used to flush an event stream.
     * @param threshold A period, measured since a first processed event after a
     * reset, after which emitter request a stream flush.
     * @param manager A @c Manager instance used to flush a stream.
     * @param scheduler A @c Scheduler instance used to schedule a stream flush.
     * @param emitter A wrapped @c Emitter instance.
     */
    TimedEmitter(StreamKey streamKey, std::chrono::milliseconds threshold,
        Manager &manager, Scheduler &scheduler,
        EmitterPtr<T> emitter = std::make_unique<FalseEmitter<T>>());

    /**
     * Destructor. Cancels a scheduled stream flush.
     */
    ~TimedEmitter();

    /**
     * Schedules a stream flush for the first event processed after a reset.
     * Forwards call to the chained emitter.
     * @see Emitter:process(EventPtr<T> event)
     */
    EventPtr<T> process(EventPtr<T> event) override;

    /**
     * Forwards call to the chained emitter.
     * @see Emitter::ready()
     */
    bool ready() override;

    /**
     * Cancels a scheduled stream flush and forwards call to the chained
     * emitter.
     * @see Emitter::reset()
     */
    void reset() override;

private:
    StreamKey m_streamKey;
    std::chrono::milliseconds m_threshold;
    Manager &m_manager;
    Scheduler &m_scheduler;
    EmitterPtr<T> m_emitter;
    bool m_periodicTriggerScheduled = false;
    std::function<void()> m_cancelPeriodicTrigger = [] {};
};

template <class T, class Scheduler>
TimedEmitter<T, Scheduler>::TimedEmitter(StreamKey streamKey,
    std::chrono::milliseconds threshold, Manager &manager, Scheduler &scheduler,
    EmitterPtr<T> emitter)
    : m_streamKey{streamKey}
    , m_threshold{threshold}
    , m_manager{manager}
    , m_scheduler{scheduler}
    , m_emitter{std::move(emitter)}
{
}

template <class T, class Scheduler> TimedEmitter<T, Scheduler>::~TimedEmitter()
{
    m_cancelPeriodicTrigger();
}

template <class T, class Scheduler>
EventPtr<T> TimedEmitter<T, Scheduler>::process(EventPtr<T> event)
{
    if (!m_periodicTriggerScheduled) {
        m_cancelPeriodicTrigger = m_scheduler.schedule(
            m_threshold, [this] { m_manager.flush(m_streamKey); });
        m_periodicTriggerScheduled = true;
    }
    return m_emitter->process(std::move(event));
}

template <class T, class Scheduler> bool TimedEmitter<T, Scheduler>::ready()
{
    return m_emitter->ready();
}

template <class T, class Scheduler> void TimedEmitter<T, Scheduler>::reset()
{
    m_cancelPeriodicTrigger();
    m_periodicTriggerScheduled = false;
    m_emitter->reset();
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_TIMED_EMITTER_H
