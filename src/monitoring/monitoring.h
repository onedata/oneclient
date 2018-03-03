/**
 * @file monitoring.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "cppmetrics/cppmetrics.h"
#include "monitoring/metricsCollector.h"

#define ONE_METRIC_PREFIX(name) name

#define ONE_MONITORING_ENABLED()                                               \
    (one::monitoring::MetricsCollector::isEnabled())

#define ONE_MONITORING_COLLECTOR()                                             \
    (one::monitoring::MetricsCollector::getInstance())

#define ONE_METRIC_REGISTRY()                                                  \
    (cppmetrics::core::MetricRegistry::DEFAULT_REGISTRY())

#define ONE_METRIC_COUNTER_INC(name)                                           \
    if (ONE_MONITORING_ENABLED()) {                                            \
        ONE_METRIC_REGISTRY()->counter(ONE_METRIC_PREFIX(name))->increment();  \
    }

#define ONE_METRIC_COUNTER_ADD(name, value)                                    \
    if (ONE_MONITORING_ENABLED()) {                                            \
        ONE_METRIC_REGISTRY()                                                  \
            ->counter(ONE_METRIC_PREFIX(name))                                 \
            ->increment(value);                                                \
    }

#define ONE_METRIC_COUNTER_DEC(name)                                           \
    if (ONE_MONITORING_ENABLED()) {                                            \
        ONE_METRIC_REGISTRY()->counter(ONE_METRIC_PREFIX(name))->decrement();  \
    }

#define ONE_METRIC_COUNTER_SUB(name, value)                                    \
    if (ONE_MONITORING_ENABLED()) {                                            \
        ONE_METRIC_REGISTRY()                                                  \
            ->counter(ONE_METRIC_PREFIX(name))                                 \
            ->decrement(value);                                                \
    }

#define ONE_METRIC_COUNTER_SET(name, value)                                    \
    if (ONE_MONITORING_ENABLED()) {                                            \
        ONE_METRIC_REGISTRY()                                                  \
            ->counter(ONE_METRIC_PREFIX(name))                                 \
            ->setCount(value);                                                 \
    }

#define ONE_METRIC_TIMER(name)                                                 \
    ONE_METRIC_REGISTRY()->timer(ONE_METRIC_PREFIX(name))

#define ONE_METRIC_TIMERCTX_START(name)                                        \
    {                                                                          \
        cppmetrics::core::TimerContextPtr __metric_timer##__COUNTER__(         \
            ONE_MONITORING_ENABLED()                                           \
                ? ONE_METRIC_TIMER(name)->timerContextPtr()                    \
                : nullptr);

#define ONE_METRIC_TIMERCTX_END(name) }

#define ONE_METRIC_TIMERCTX_CREATE(name)                                       \
    (ONE_MONITORING_ENABLED()                                                  \
            ? ONE_METRIC_TIMER(name)->timerContextPtr()                        \
            : std::shared_ptr<cppmetrics::core::TimerContextBase>(nullptr))

#define ONE_METRIC_TIMERCTX_STOP(timerctxptr, count)                           \
    if (timerctxptr) {                                                         \
        timerctxptr->stop(count);                                              \
    }

#define ONE_METRIC_TIMERCTX_DESTROY(timerctxptr) timerctxptr.reset();
