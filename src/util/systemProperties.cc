/**
 * @file systemProperties.cc
 * @author Bartek Kryza
 * @copyright (C) 2026 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "systemProperties.h"

#include <Poco/Environment.h>
#include <sys/sysinfo.h>

namespace one {
namespace client {
namespace util {

std::vector<std::pair<std::string, std::string>> getSystemProperties()
{
    std::vector<std::pair<std::string, std::string>> props;

    props.emplace_back("os.name", Poco::Environment::osName());
    props.emplace_back("os.display_name", Poco::Environment::osDisplayName());
    props.emplace_back("os.version", Poco::Environment::osVersion());
    props.emplace_back("os.arch", Poco::Environment::osArchitecture());
    props.emplace_back("node.name", Poco::Environment::nodeName());
    props.emplace_back(
        "processor.count", std::to_string(Poco::Environment::processorCount()));

    struct sysinfo si {
    };
    if (sysinfo(&si) == 0) {
        props.emplace_back("memory.total_kb",
            std::to_string(
                static_cast<unsigned long>(si.totalram) * si.mem_unit / 1024));
        props.emplace_back("memory.available_kb",
            std::to_string(
                static_cast<unsigned long>(si.freeram + si.bufferram) *
                si.mem_unit / 1024));
    }

    return props;
}

} // namespace util
} // namespace client
} // namespace one
