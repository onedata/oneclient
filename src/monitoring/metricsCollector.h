/**
 * @file metricsCollector.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "cppmetrics/cppmetrics.h"
#include "monitoring/monitoringConfiguration.h"

namespace one {
namespace monitoring {

using namespace cppmetrics;
/**
 * MetricsCollector is an abstract class responsible for managing performance
 * metrics collection in 'helpers' and 'oneclient' and reporting them to
 * Graphite or console.
 */
class MetricsCollector {
public:
    /**
     * Default constructor.
     * @param conf Monitoring configuration options.
     */
    MetricsCollector();

    virtual ~MetricsCollector();

    void setConfiguration(
        std::shared_ptr<MonitoringConfiguration> monitoringConfiguration);

    const MonitoringConfiguration &getConfiguration() const;

    virtual void initialize();

    virtual void start();

    virtual void stop();

    core::MetricRegistryPtr getRegistry();

    template <typename TMetricsCollector = MetricsCollector>
    static std::shared_ptr<MetricsCollector> getInstance()
    {
        if (!m_singleton) {
            m_singleton = std::dynamic_pointer_cast<MetricsCollector>(
                std::make_shared<TMetricsCollector>());
            m_isEnabled = true;
        }

        return m_singleton;
    }

    static bool isEnabled() { return m_isEnabled; }

protected:
    std::shared_ptr<MonitoringConfiguration> m_conf;
    std::shared_ptr<cppmetrics::core::ScheduledReporter> m_reporter;

private:
    static std::shared_ptr<MetricsCollector> m_singleton;
    static bool m_isEnabled;
};

} // namespace monitoring
} // namespace one
