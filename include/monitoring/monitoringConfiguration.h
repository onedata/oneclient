/**
 * @file monitoringConfiguration.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "cppmetrics/core/scheduled_reporter.h"

#include <string>
#include <unistd.h>

#include <folly/Uri.h>

namespace one {
namespace monitoring {

/**
 * Default performance metrics monitoring reporting period in seconds.
 */
constexpr auto REPORTING_PERIOD = 30;

/**
 * Default Graphite server port.
 */
constexpr auto GRAPHITE_PORT = 2003;

/**
 * Graphite namespace separator.
 */
constexpr auto GRAPHITE_NAMESPACE_SEPARATOR = ".";

/**
 * Default reporting level.
 */
constexpr auto REPORTING_LEVEL = cppmetrics::core::ReportingLevel::Basic;

/**
 * Generic monitoring configuration settings
 */
class MonitoringConfiguration {
public:
    /**
     * Reporting period in seconds.
     */
    uint32_t reportingPeriod = REPORTING_PERIOD;

    /**
     * Monitoring reporting level.
     */
    cppmetrics::core::ReportingLevel reportingLevel = REPORTING_LEVEL;

    /**
     * The prefix prepended to each metric name.
     */
    std::string namespacePrefix;

    /**
     * Default separator of the metric name tokens.
     */
    virtual std::string getNamespaceSeparator() const
    {
        return GRAPHITE_NAMESPACE_SEPARATOR;
    }
};

/**
 * Monitoring configuration settings specific for Graphite reporter.
 */
class GraphiteMonitoringConfiguration : public MonitoringConfiguration {
public:
    enum class GraphiteProtocol { UDP, TCP };

    /**
     * Graphite protocol (either UDP or TCP)
     */
    GraphiteProtocol graphiteProtocol = GraphiteProtocol::TCP;

    /**
     * Hostname of the Graphite server.
     */
    std::string graphiteHostname;

    /**
     * Port of the Graphite server.
     */
    uint32_t graphitePort = GRAPHITE_PORT;

    /**
     * Initialize Graphite endpoint configuration from URL. URL should have the
     * form e.g. tcp://192.168.1.2:2003
     */
    void fromGraphiteURL(std::string url)
    {
        folly::Uri folly(url);

        // Assume TCP as default scheme
        if (folly.scheme().empty() || folly.scheme() == "tcp") {
            graphiteProtocol = GraphiteProtocol::TCP;
        }
        else if (folly.scheme() == "udp") {
            graphiteProtocol = GraphiteProtocol::UDP;
        }
        else {
            throw std::invalid_argument(
                "Unsupported Graphite protocol provided: " + url);
        }

        graphiteHostname = folly.host();
        if (folly.port())
            graphitePort = folly.port();
    }
};
}
}
