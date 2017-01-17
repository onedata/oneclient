/**
 * @file optionsParser.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_OPTIONS_PARSER_H
#define ONECLIENT_OPTIONS_PARSER_H

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include <vector>

namespace one {
namespace client {
namespace options {

class Option;

/**
 * @c OptionsParser is responsible for parsing options from a command line,
 * an environment and a configuration file.
 */
class OptionsParser {
public:
    /**
     * Construstor.
     * @param options List of supported options.
     */
    OptionsParser(const std::vector<std::shared_ptr<Option>> &options);

    /**
     * Parses command line options and stores them in the variables map.
     * @param argc Number of command line arguments.
     * @param argv An array of command line arguments.
     * @param vm A variables map in which store parsing results.
     */
    void parseCommandLine(const int argc, const char *const argv[],
        boost::program_options::variables_map &vm);

    /**
     * Parses environment options and stores them in the variables map.
     * Stores unprefix, deprecated options in @c deprecated container.
     * @param A deprecated container in which store unprefix environment
     * options.
     * @param A variables map in which store parsing results.
     */
    void parseEnvironment(std::vector<std::string> &deprecated,
        boost::program_options::variables_map &vm);

    /**
     * Parses configuration file options and stores them in the variables map.
     * @param path A path to the configuration file.
     * @param vm A variables map in which store parsing results.
     */
    void parseConfigFile(const boost::filesystem::path &path,
        boost::program_options::variables_map &vm);

private:
    const std::vector<std::shared_ptr<Option>> &m_options;
};

} // namespace options
} // namespace client
} // namespace one

#endif // ONECLIENT_OPTIONS_PARSER_H
