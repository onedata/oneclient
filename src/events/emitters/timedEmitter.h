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

template <class T> class TimedEmitter : public Emitter<T> {
public:
    TimedEmitter(StreamKey streamKey, std::chrono::milliseconds threshold,
        Manager &manager, Scheduler &scheduler,
        EmitterPtr<T> emitter = std::make_unique<FalseEmitter<T>>());

    ~TimedEmitter();

    EventPtr<T> process(EventPtr<T> event) override;

    bool ready() override;

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

template <class T>
TimedEmitter<T>::TimedEmitter(StreamKey streamKey,
    std::chrono::milliseconds threshold, Manager &manager, Scheduler &scheduler,
    EmitterPtr<T> emitter)
    : m_streamKey{streamKey}
    , m_threshold{threshold}
    , m_manager{manager}
    , m_scheduler{scheduler}
    , m_emitter{std::move(emitter)}
{
}

template <class T> TimedEmitter<T>::~TimedEmitter()
{
    m_cancelPeriodicTrigger();
}

template <class T> EventPtr<T> TimedEmitter<T>::process(EventPtr<T> event)
{
    if (!m_periodicTriggerScheduled) {
        m_cancelPeriodicTrigger = m_scheduler.schedule(
            m_threshold, [this] { m_manager.flush(m_streamKey); });
        m_periodicTriggerScheduled = true;
    }
    return m_emitter->process(std::move(event));
}

template <class T> bool TimedEmitter<T>::ready() { return m_emitter->ready(); }

template <class T> void TimedEmitter<T>::reset()
{
    m_cancelPeriodicTrigger();
    m_periodicTriggerScheduled = false;
    m_emitter->reset();
}

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_TIMED_EMITTER_H
