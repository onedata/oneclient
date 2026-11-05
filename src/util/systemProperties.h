/**
 * @file systemProperties.h
 * @author Bartek Kryza
 * @copyright (C) 2026 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <string>
#include <utility>
#include <vector>

namespace one {
namespace client {
namespace util {

/**
 * Returns a list of key-value pairs describing the most relevant system
 * properties of the environment in which the process is running.
 *
 * Properties include OS name, OS version, architecture, node name, processor
 * count, and total/available memory (via sysinfo).
 *
 * @return Vector of (property name, value) pairs.
 */
std::vector<std::pair<std::string, std::string>> getSystemProperties();

} // namespace util
} // namespace client
} // namespace one
