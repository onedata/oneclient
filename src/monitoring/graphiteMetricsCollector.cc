/**
 * @file graphiteMetricsCollector.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include <memory>

#include "cppmetrics/cppmetrics.h"
#include "logging.h"
#include "monitoring/graphiteMetricsCollector.h"
#include "monitoring/monitoringConfiguration.h"

namespace one {
namespace monitoring {

GraphiteMetricsCollector::GraphiteMetricsCollector() {}

GraphiteMetricsCollector::~GraphiteMetricsCollector() {}

void GraphiteMetricsCollector::initialize()
{
    LOG_FCALL();

    auto conf =
        std::dynamic_pointer_cast<GraphiteMonitoringConfiguration>(m_conf);

    LOG_DBG(1) << "Initializing Graphite metrics reporter";

    if (!conf) {
        LOG_DBG(1) << "Invalid Graphite reporter configuration";
        throw std::runtime_error("Invalid monitoring configuration type");
    }

    if (conf->graphiteProtocol ==
        GraphiteMonitoringConfiguration::GraphiteProtocol::TCP) {
        LOG_DBG(1) << "Creating TCP Graphite reporter";
        m_sender.reset(new graphite::GraphiteSenderTCP(
            conf->graphiteHostname, conf->graphitePort));
    }
    else {
        LOG_DBG(1) << "Creating UDP Graphite reporter";
        m_sender.reset(new graphite::GraphiteSenderUDP(
            conf->graphiteHostname, conf->graphitePort));
    }

    m_reporter.reset(new graphite::GraphiteReporter(
        getRegistry(), m_sender, conf->namespacePrefix));

    m_reporter->setReportingLevel(conf->reportingLevel);
}
} // namespace monitoring
} // namespace one
