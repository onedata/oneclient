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
    uint32_t reportingPeriod = REPORTING_PERIOD;
    cppmetrics::core::ReportingLevel reportingLevel = REPORTING_LEVEL;
    std::string namespaceRoot;
    std::string namespaceHost;
    std::string namespaceContainer;

    virtual std::string getPrefix() const
    {
        std::string result;
        result += namespaceRoot;
        result += getNamespaceSeparator();
        result += "host";
        result += getNamespaceSeparator();
        result += namespaceHost;
        result += getNamespaceSeparator();
        if (!namespaceContainer.empty()) {
            result += "container";
            result += getNamespaceSeparator();
            result += namespaceContainer;
            result += getNamespaceSeparator();
        }
        result += "uid";
        result += getNamespaceSeparator();
        result += std::to_string(getuid());

        return result;
    }

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

    GraphiteProtocol graphiteProtocol = GraphiteProtocol::TCP;
    std::string graphiteHostname;
    uint32_t graphitePort = GRAPHITE_PORT;

    void fromGraphiteURL(std::string url)
    {
        folly::Uri folly(url);

        if (folly.scheme() == "tcp")
            graphiteProtocol = GraphiteProtocol::TCP;
        else if (folly.scheme() == "udp")
            graphiteProtocol = GraphiteProtocol::UDP;
        else
            throw std::invalid_argument(
                "Unsupported Graphite protocol: " + folly.scheme());

        graphiteHostname = folly.host();
        if (folly.port())
            graphitePort = folly.port();
    }
};
}
}
