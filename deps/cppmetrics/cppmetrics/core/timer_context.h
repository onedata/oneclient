/*
 * Copyright 2000-2014 NeuStar, Inc. All rights reserved.
 * NeuStar, the Neustar logo and related names and logos are registered
 * trademarks, service marks or tradenames of NeuStar, Inc. All other
 * product names, company names, marks, logos and symbols may be trademarks
 * of their respective owners.
 */

/*
 * timer_context.h
 *
 *  Created on: Jun 5, 2014
 *      Author: vpoliboy
 */

#ifndef TIMER_CONTEXT_H_
#define TIMER_CONTEXT_H_

#include "cppmetrics/core/types.h"
#include <chrono>
#include <memory>

namespace cppmetrics {
namespace core {

class Timer;

/**
 * Void timer context used to mock timers when monitoring is disabled.
 */
class TimerContextBase {
public:
    TimerContextBase() {}

    virtual ~TimerContextBase() {}

    virtual void reset() {}

    virtual std::chrono::nanoseconds stop(uint64_t count = 1) {}
};

/**
 * Class that actually measures the wallclock time.
 */
class TimerContext : public TimerContextBase {
public:
    /**
     * Creates a TimerContext.
     * @param timer The parent timer metric.
     */
    TimerContext(Timer &timer);

    virtual ~TimerContext();

    TimerContext &operator=(const TimerContext &) = delete;

    /**
     * Resets the underlying clock.
     */
    virtual void reset() override;

    /**
     * Stops recording the elapsed time and updates the timer.
     * @return the elapsed time in nanoseconds
     */
    virtual std::chrono::nanoseconds stop(uint64_t count = 1) override;

private:
    Clock::time_point start_time_; ///< The start time on instantitation */
    Timer &timer_;                 ///< The parent timer object. */
    bool active_;                  ///< Whether the timer is active or not */
};

typedef std::shared_ptr<TimerContext> TimerContextPtr;

} /* namespace cppmetrics */
} /* namespace core */
#endif /* TIMER_CONTEXT_H_ */
