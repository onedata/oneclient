/**
 * @file logging.h
 * @author Bartek Kryza
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "options/options.h"

#include <memory>

namespace one {
namespace client {
namespace logging {

/**
 * Starts logging to directory specified in the options.
 *
 * @param programName Program name which will be used as the prefix for
 *                    generated log files.
 * @param options Command line options passed to the oneclient or onedatafs.
 */
void startLogging(
    const char *programName, std::shared_ptr<options::Options> options);

/**
 * Starts performance metrics logging.
 *
 * @param options Command line options passed to the oneclient or onedatafs.
 */
int startPerformanceMonitoring(std::shared_ptr<options::Options> options);

} // namespace logging
} // namespace client
} // namespace one
