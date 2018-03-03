/**
 * @file init.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_INIT_H
#define HELPERS_INIT_H

#include "monitoring/monitoringConfiguration.h"

namespace one {
namespace helpers {
/**
 * Helpers initialization function, should initialize any dependent
 * libraries e.g. folly.
 *
 * Should be called before any other helpers method or function.
 */
void init(void);

/**
 * Initialize the monitoring configuration
 *
 * @param conf Monitoring configuration necessary to setup Graphite
 *             or console reporters.
 * @param start Flag determining whether reporting should start automatically
 *              after configuration
 */
void configureMonitoring(
    std::shared_ptr<one::monitoring::MonitoringConfiguration> conf,
    bool start = false);
}
}

#endif /* HELPERS_INIT_H */
