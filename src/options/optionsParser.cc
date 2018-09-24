/**
 * @file optionsParser.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "optionsParser.h"
#include "option.h"
#include "options.h"

#include <boost/algorithm/string.hpp>

#include <fstream>

namespace one {
namespace client {
namespace options {

OptionsParser::OptionsParser(
    const std::vector<std::shared_ptr<Option>> &options)
    : m_options{options}
{
}

void OptionsParser::parseCommandLine(const int argc, const char *const argv[],
    boost::program_options::variables_map &vm)
{
    boost::program_options::options_description desc;
    boost::program_options::positional_options_description pos_desc;

    for (const auto &option : m_options) {
        option->addCommandLine(desc);
        option->addPositional(pos_desc);
    }

    boost::program_options::store(
        boost::program_options::command_line_parser(argc, argv)
            .options(desc)
            .positional(pos_desc)
            .run(),
        vm);
}

void OptionsParser::parseEnvironment(std::vector<std::string> &deprecated,
    boost::program_options::variables_map &vm)
{
    boost::program_options::options_description desc;
    for (const auto &option : m_options) {
        option->addEnvironment(desc);
    }

    boost::program_options::store(
        boost::program_options::parse_environment(desc,
            [&](const std::string &env) -> std::string {
                auto envCopy = env;
                boost::algorithm::to_upper(envCopy);

                bool prefixed = false;
                if (envCopy.find(ENVIRONMENT_PREFIX) == 0) {
                    prefixed = true;
                    envCopy = envCopy.substr(std::strlen(ENVIRONMENT_PREFIX));
                }

                boost::algorithm::to_lower(envCopy);

                if ((desc.find_nothrow(envCopy, false) != nullptr) &&
                    (vm.count(envCopy) == 0u || vm.at(envCopy).defaulted())) {
                    if (!prefixed) {
                        deprecated.push_back(env);
                    }
                    return envCopy;
                }

                return {};
            }),
        vm);
}

void OptionsParser::parseConfigFile(const boost::filesystem::path &path,
    boost::program_options::variables_map &vm)
{
    boost::program_options::options_description desc;
    for (const auto &option : m_options) {
        option->addConfigFile(desc);
    }

    std::ifstream file{path.c_str()};
    boost::program_options::store(
        boost::program_options::parse_config_file(file, desc), vm);
}

} // namespace options
} // namespace client
} // namespace one
