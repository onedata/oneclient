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
#include "falseEmitter.h"

#include <chrono>
#include <functional>

namespace one {
class Scheduler;
namespace client {
namespace events {

class Manager;

class TimedEmitter : public Emitter {
public:
    TimedEmitter(std::int64_t streamId, std::chrono::milliseconds threshold,
        Manager &manager, Scheduler &scheduler,
        std::unique_ptr<Emitter> emitter = std::make_unique<FalseEmitter>());

    ~TimedEmitter();

    void process(ConstEventPtr event) override;

    bool ready() override;

    void reset() override;

private:
    std::int64_t m_streamId;
    std::chrono::milliseconds m_threshold;
    Manager &m_manager;
    Scheduler &m_scheduler;
    std::unique_ptr<Emitter> m_emitter;
    bool periodicTriggerScheduled = false;
    std::function<void()> m_cancelPeriodicTrigger = [] {};
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_EMITTERS_TIMED_EMITTER_H
