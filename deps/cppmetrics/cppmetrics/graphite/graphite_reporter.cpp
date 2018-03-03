/*
 * Copyright 2000-2014 NeuStar, Inc. All rights reserved.
 * NeuStar, the Neustar logo and related names and logos are registered
 * trademarks, service marks or tradenames of NeuStar, Inc. All other
 * product names, company names, marks, logos and symbols may be trademarks
 * of their respective owners.
 */

/*
 * graphite_reporter.cpp
 *
 *  Created on: Jun 12, 2014
 *      Author: vpoliboy
 */

#include "cppmetrics/graphite/graphite_reporter.h"
#include "cppmetrics/core/utils.h"
#include <glog/logging.h>

namespace cppmetrics {
namespace graphite {

GraphiteReporter::GraphiteReporter(core::MetricRegistryPtr registry,
    GraphiteSenderPtr sender, std::string prefix,
    std::chrono::milliseconds rate_unit)
    : ScheduledReporter(registry, rate_unit)
    , sender_(sender)
    , prefix_(prefix)
{
}

GraphiteReporter::~GraphiteReporter() {}

template <typename T> std::string GraphiteReporter::format(T o)
{
    return std::to_string(o);
}

void GraphiteReporter::report(core::CounterMap counter_map,
    core::HistogramMap histogram_map, core::MeteredMap meter_map,
    core::TimerMap timer_map, core::GaugeMap gauge_map)
{
    auto timestamp = core::get_seconds_from_epoch();

    try {
        sender_->connect();

        for (const auto &kv : counter_map) {
            reportCounter(kv.first, kv.second, timestamp);
        }

        for (const auto &kv : histogram_map) {
            reportHistogram(kv.first, kv.second, timestamp);
        }

        for (const auto &kv : meter_map) {
            reportMeter(kv.first, kv.second, timestamp);
        }

        for (const auto &kv : timer_map) {
            reportTimer(kv.first, kv.second, timestamp);
        }

        for (const auto &kv : gauge_map) {
            reportGauge(kv.first, kv.second, timestamp);
        }

        sender_->close();
    }
    catch (const std::exception &e) {
        LOG(ERROR) << "Exception in graphite reporting: " << e.what();
        sender_->close();
    }
}

void GraphiteReporter::reportTimer(
    const std::string &name, core::TimerPtr timer, uint64_t timestamp)
{
    core::SnapshotPtr snapshot = timer->getSnapshot();

    sender_->send(
        prefix(name, "count"), format(timer->getMeterCount()), timestamp);
    sender_->send(prefix(name, "duration"),
        format(convertDurationUnit(timer->getDuration())), timestamp);
    if (getReportingLevel() == cppmetrics::core::ReportingLevel::Full) {
        sender_->send(prefix(name, "getMax"),
            format(convertDurationUnit(snapshot->getMax())), timestamp);
        sender_->send(prefix(name, "mean"),
            format(convertDurationUnit(snapshot->getMean())), timestamp);
        sender_->send(prefix(name, "min"),
            format(convertDurationUnit(snapshot->getMin())), timestamp);
        sender_->send(prefix(name, "stddev"),
            format(convertDurationUnit(snapshot->getStdDev())), timestamp);
        sender_->send(prefix(name, "p50"),
            format(convertDurationUnit(snapshot->getMedian())), timestamp);
        sender_->send(prefix(name, "p75"),
            format(convertDurationUnit(snapshot->get75thPercentile())),
            timestamp);
        sender_->send(prefix(name, "p95"),
            format(convertDurationUnit(snapshot->get95thPercentile())),
            timestamp);
        sender_->send(prefix(name, "p98"),
            format(convertDurationUnit(snapshot->get98thPercentile())),
            timestamp);
        sender_->send(prefix(name, "p99"),
            format(convertDurationUnit(snapshot->get99thPercentile())),
            timestamp);
        sender_->send(prefix(name, "p999"),
            format(convertDurationUnit(snapshot->get999thPercentile())),
            timestamp);
    }

    reportMeter(
        name, std::static_pointer_cast<core::Metered>(timer), timestamp);
}

void GraphiteReporter::reportMeter(
    const std::string &name, core::MeteredPtr meter, uint64_t timestamp)
{
    sender_->send(prefix(name, "events"), format(meter->getCount()), timestamp);

    if (getReportingLevel() == cppmetrics::core::ReportingLevel::Full) {
        sender_->send(prefix(name, "m1_rate"),
            format(convertRateUnit(meter->getOneMinuteRate())), timestamp);
        sender_->send(prefix(name, "m5_rate"),
            format(convertRateUnit(meter->getFiveMinuteRate())), timestamp);
        sender_->send(prefix(name, "m15_rate"),
            format(convertRateUnit(meter->getFifteenMinuteRate())), timestamp);
        sender_->send(prefix(name, "mean_rate"),
            format(convertRateUnit(meter->getMeanRate())), timestamp);
    }
}

void GraphiteReporter::reportHistogram(
    const std::string &name, core::HistogramPtr histogram, uint64_t timestamp)
{
    core::SnapshotPtr snapshot = histogram->getSnapshot();

    sender_->send(
        prefix(name, "count"), format(histogram->getCount()), timestamp);
    sender_->send(
        prefix(name, "getMax"), format(snapshot->getMax()), timestamp);
    sender_->send(prefix(name, "mean"), format(snapshot->getMean()), timestamp);
    sender_->send(prefix(name, "min"), format(snapshot->getMin()), timestamp);
    if (getReportingLevel() == cppmetrics::core::ReportingLevel::Full) {

        sender_->send(
            prefix(name, "stddev"), format(snapshot->getStdDev()), timestamp);
        sender_->send(
            prefix(name, "p50"), format(snapshot->getMedian()), timestamp);
        sender_->send(prefix(name, "p75"),
            format(snapshot->get75thPercentile()), timestamp);
        sender_->send(prefix(name, "p95"),
            format(snapshot->get95thPercentile()), timestamp);
        sender_->send(prefix(name, "p98"),
            format(snapshot->get98thPercentile()), timestamp);
        sender_->send(prefix(name, "p99"),
            format(snapshot->get99thPercentile()), timestamp);
        sender_->send(prefix(name, "p999"),
            format(snapshot->get999thPercentile()), timestamp);
    }
}

void GraphiteReporter::reportCounter(
    const std::string &name, core::CounterPtr counter, uint64_t timestamp)
{
    sender_->send(
        prefix(name, "count"), format(counter->getCount()), timestamp);
}

void GraphiteReporter::reportGauge(
    const std::string &name, core::GaugePtr gauge, uint64_t timestamp)
{
    const std::string value = format(gauge->getValue());
    sender_->send(
        prefix(name), value, timestamp, GraphiteSender::metric_t::Gauge_t);
}

std::string GraphiteReporter::prefix(const std::string &name, const char *extra)
{
    std::string result(prefix_ + '.' + name);
    return extra ? (result + '.' + extra) : result;
}

} /* namespace graphite */
} /* namespace cppmetrics */
