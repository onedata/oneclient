/**
 * @file graphiteMetricsCollector.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "monitoring/metricsCollector.h"

namespace one {
namespace monitoring {

/**
 * GraphiteMetricsCollector is a Graphite specific monitoring collector.
 */
class GraphiteMetricsCollector : public MetricsCollector {
public:
    /**
     * Default constructor.
     */
    GraphiteMetricsCollector();

    /**
     * Destructor.
     */
    virtual ~GraphiteMetricsCollector();

    /**
     * Provide Graphite specific initialization. The metrics collector has to
     * be first configured with a proper GraphiteMonitoringConfiguration
     * instance.
     */
    virtual void initialize() override;

private:
    std::shared_ptr<cppmetrics::graphite::GraphiteSender> m_sender;
};

} // namespace monitoring
} // namespace one
