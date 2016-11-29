/**
 * @file timedEmitter.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "timedEmitter.h"
#include "events/manager.h"
#include "scheduler.h"

namespace one {
namespace client {
namespace events {

TimedEmitter::TimedEmitter(std::int64_t streamId,
    std::chrono::milliseconds threshold, Manager &manager, Scheduler &scheduler,
    std::unique_ptr<Emitter> emitter)
    : m_streamId{streamId}
    , m_threshold{threshold}
    , m_manager{manager}
    , m_scheduler{scheduler}
    , m_emitter{std::move(emitter)}
{
}

TimedEmitter::~TimedEmitter() { m_cancelPeriodicTrigger(); }

void TimedEmitter::process(ConstEventPtr event)
{
    if (!periodicTriggerScheduled) {
        m_cancelPeriodicTrigger = m_scheduler.schedule(
            m_threshold, [this] { m_manager.flush(m_streamId); });
        periodicTriggerScheduled = true;
    }
    m_emitter->process(std::move(event));
}

bool TimedEmitter::ready() { return m_emitter->ready(); }

void TimedEmitter::reset()
{
    m_cancelPeriodicTrigger();
    periodicTriggerScheduled = false;
    m_emitter->reset();
}

} // namespace events
} // namespace client
} // namespace one
