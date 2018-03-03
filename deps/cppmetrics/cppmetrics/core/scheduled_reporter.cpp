/*
 * Copyright 2000-2014 NeuStar, Inc. All rights reserved.
 * NeuStar, the Neustar logo and related names and logos are registered
 * trademarks, service marks or tradenames of NeuStar, Inc. All other
 * product names, company names, marks, logos and symbols may be trademarks
 * of their respective owners.
 */

/*
 * scheduled_reporter.cpp
 *
 *  Created on: Jun 10, 2014
 *      Author: vpoliboy
 */

#include "cppmetrics/core/scheduled_reporter.h"
#include <glog/logging.h>

namespace cppmetrics {
namespace core {

ScheduledReporter::ScheduledReporter(
    MetricRegistryPtr registry, std::chrono::milliseconds rate_unit)
    : running_(false)
    , metric_registry_(registry)
    , scheduled_executor_(2)
    , rate_factor_(std::chrono::milliseconds(1000).count() / rate_unit.count())
    , duration_factor_(static_cast<double>(1.0) /
          std::chrono::duration_cast<std::chrono::nanoseconds>(
              std::chrono::milliseconds(1))
              .count())
{
}

ScheduledReporter::~ScheduledReporter() { stop(); }

void ScheduledReporter::report()
{
    CounterMap counter_map(metric_registry_->getCounters());
    HistogramMap histogram_map(metric_registry_->getHistograms());
    MeteredMap meter_map(metric_registry_->getMeters());
    TimerMap timer_map(metric_registry_->getTimers());
    GaugeMap gauge_map(metric_registry_->getGauges());
    report(counter_map, histogram_map, meter_map, timer_map, gauge_map);
}

void ScheduledReporter::start(std::chrono::milliseconds period)
{
    if (!running_) {
        running_ = true;
        scheduled_executor_.scheduleAtFixedDelay(
            [s = std::weak_ptr<ScheduledReporter>{shared_from_this()}]() {
                auto self = s.lock();
                if (!self)
                    return;
                self->report();
            },
            period);
    }
}

void ScheduledReporter::stop()
{
    if (running_) {
        running_ = false;
        scheduled_executor_.shutdownNow();
    }
}

std::string ScheduledReporter::rateUnitInSec() const
{
    std::ostringstream ostrstr;
    ostrstr << rate_factor_;
    ostrstr << " Seconds";
    return ostrstr.str();
}

double ScheduledReporter::convertDurationUnit(double duration) const
{
    return duration * duration_factor_;
}

double ScheduledReporter::convertRateUnit(double rate) const
{
    return rate * rate_factor_;
}

void ScheduledReporter::setReportingLevel(ReportingLevel level) {
    reporting_level_ = level;
}

ReportingLevel ScheduledReporter::getReportingLevel() const {
    return reporting_level_;
}

} /* namespace core */
} /* namespace cppmetrics */
