/**
 * @file metricsCollector.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include <memory>

#include "cppmetrics/cppmetrics.h"
#include "logging.h"
#include "monitoring/graphiteMetricsCollector.h"
#include "monitoring/metricsCollector.h"

namespace one {
namespace monitoring {

class GraphiteMetricsCollector;

std::shared_ptr<MetricsCollector> MetricsCollector::m_singleton{nullptr};
bool MetricsCollector::m_isEnabled{false};

MetricsCollector::MetricsCollector() {}

MetricsCollector::~MetricsCollector() {}

void MetricsCollector::setConfiguration(
    std::shared_ptr<MonitoringConfiguration> monitoringConfiguration)
{
    LOG_FCALL();

    m_conf = monitoringConfiguration;
    initialize();
}

const MonitoringConfiguration &MetricsCollector::getConfiguration() const
{
    return *m_conf;
}

void MetricsCollector::initialize() { LOG_FCALL(); }

void MetricsCollector::start()
{
    LOG_FCALL();

    m_reporter->start(std::chrono::seconds(m_conf->reportingPeriod));
}

void MetricsCollector::stop()
{
    LOG_FCALL();

    m_reporter->stop();
}

core::MetricRegistryPtr MetricsCollector::getRegistry()
{
    return core::MetricRegistry::DEFAULT_REGISTRY();
}
} // namespace monitoring
} // namespace one
