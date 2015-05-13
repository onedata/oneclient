/**
 * @file options.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "options.h"

#include "version.h"
#include "logging.h"
#include "oneException.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>
#include <boost/xpressive/xpressive.hpp>

#include <fstream>
#include <functional>
#include <sstream>
#include <vector>
#include <utility>

using namespace boost::program_options;

namespace one {
namespace client {

Options::Options()
    : m_common("Common config file and environment options")
    , m_restricted("Global config file restricted options")
    , m_commandline("General options")
    , m_fuse("FUSE options")
    , m_hidden("Hidden commandline options")
{
    setDescriptions();
}

Options::~Options() {}

void Options::setDescriptions()
{
    // Common options found in environment, global and user config files
    add_provider_hostname(m_common);
    add_provider_port(m_common);
    add_peer_certificate_file(m_common);
    add_no_check_certificate(m_common);
    add_fuse_group_id(m_common);
    add_enable_attr_cache(m_common);
    add_attr_cache_expiration_time(m_common);
    add_log_dir(m_common);
    add_fuse_id(m_common);
    add_jobscheduler_threads(m_common);
    add_enable_dir_prefetch(m_common);
    add_enable_parallel_getattr(m_common);
    add_enable_permission_checking(m_common);
    add_enable_location_cache(m_common);
    add_global_registry_url(m_common);
    add_global_registry_port(m_common);
    add_authentication(m_common);

    // Restricted options exclusive to global config file
    add_enable_env_option_override(m_restricted);
    add_cluster_ping_interval(m_restricted);
    add_alive_meta_connections_count(m_restricted);
    add_alive_data_connections_count(m_restricted);
    add_write_buffer_max_size(m_restricted);
    add_read_buffer_max_size(m_restricted);
    add_write_buffer_max_file_size(m_restricted);
    add_read_buffer_max_file_size(m_restricted);
    add_file_buffer_prefered_block_size(m_restricted);

    // General commandline options
    add_switch_help(m_commandline);
    add_switch_version(m_commandline);
    add_config(m_commandline);
    add_authentication(m_commandline);
    add_switch_debug(m_commandline);
    add_switch_debug_gsi(m_commandline);
    add_switch_no_check_certificate(m_commandline);

    // FUSE-specific commandline options
    m_fuse.add_options()(",o",
        value<std::vector<std::string>>()->value_name("opt,..."),
        "mount options")(",f", "foreground operation")(
        ",s", "disable multi-threaded operation");

    // Hidden commandline options (positional)
    m_hidden.add_options()("mountpoint", value<std::string>(), "mount point");
}

void Options::parseConfigs(const int argc, const char *const argv[])
{
    if (argc > 0)
        argv0 = argv[0];

    try {
        if (!parseCommandLine(argc, argv))
            return;
    }
    catch (boost::program_options::error &e) {
        LOG(ERROR) << "Error while parsing command line arguments: "
                   << e.what();
        throw OneException("", e.what());
    }

    variables_map fileConfigMap;
    try {
        parseUserConfig(fileConfigMap);
    }
    catch (boost::program_options::unknown_option &e) {
        LOG(ERROR) << "Error while parsing user configuration file: "
                   << e.what();
        if (m_restricted.find_nothrow(e.get_option_name(), false))
            throw OneException("", "restricted option '" + e.get_option_name() +
                    "' found in user configuration file");

        throw OneException("", e.what());
    }
    catch (boost::program_options::error &e) {
        LOG(ERROR) << "Error while parsing user configuration file: "
                   << e.what();
        throw OneException("", e.what());
    }

    try {
        parseGlobalConfig(fileConfigMap);
    }
    catch (boost::program_options::error &e) {
        LOG(ERROR) << "Error while parsing global configuration file: "
                   << e.what();
        throw OneException("", e.what());
    }

    // If override is allowed then we merge in environment variables first
    if (fileConfigMap.at("enable_env_option_override").as<bool>()) {
        parseEnv();
        m_vm.insert(fileConfigMap.begin(), fileConfigMap.end());
    }
    else {
        m_vm.insert(fileConfigMap.begin(), fileConfigMap.end());
        parseEnv();
    }

    notify(m_vm);
}

/**
* Parses commandline-options with dashes as commandline_options with
* underscores.
* @param str The command line argument to parse.
* @returns A pair of argument name and argument value parsed from the input
* string. The argument name has dashes replaced with underscores.
*/
static std::pair<std::string, std::string> cmdParser(const std::string &str)
{
    using namespace boost::xpressive;

    static const sregex rex =
        sregex::compile(R"(\s*--([\w\-]+)(?:=(\S+))?\s*)");

    smatch what;
    if (regex_match(str, what, rex))
        return std::make_pair(
            boost::algorithm::replace_all_copy(what[1].str(), "-", "_"),
            what.size() > 1 ? what[2].str() : std::string());

    return std::pair<std::string, std::string>();
}

bool Options::parseCommandLine(const int argc, const char *const argv[])
{
    positional_options_description pos;
    pos.add("mountpoint", 1);

    options_description all("Allowed options");
    all.add(m_commandline).add(m_fuse).add(m_hidden);

    store(command_line_parser(argc, argv)
              .options(all)
              .positional(pos)
              .extra_parser(cmdParser)
              .run(),
        m_vm);

    return !m_vm.count("help") && !m_vm.count("version");
}

void Options::parseUserConfig(variables_map &fileConfigMap)
{
    using namespace boost::filesystem;
    if (!m_vm.count("config"))
        return;

    const path userConfigPath = absolute(m_vm.at("config").as<std::string>());
    std::ifstream userConfig(userConfigPath.c_str());

    if (userConfig)
        LOG(INFO) << "Parsing user configuration file: '" << userConfigPath
                  << "'";
    else
        LOG(WARNING) << "Couldn't open user configuration file: '"
                     << userConfigPath << "'";

    store(parse_config_file(userConfig, m_common), fileConfigMap);
}

void Options::parseGlobalConfig(variables_map &fileConfigMap)
{
    using namespace boost::filesystem;

    options_description global("Global configuration");
    global.add(m_restricted).add(m_common);

    const path globalConfigPath = path(oneclient_INSTALL_PATH) /
        oneclient_CONFIG_DIR / GLOBAL_CONFIG_FILE;
    std::ifstream globalConfig(globalConfigPath.c_str());

    if (globalConfig)
        LOG(INFO) << "Parsing global configuration file: '" << globalConfigPath
                  << "'";
    else
        LOG(WARNING) << "Couldn't open global configuration file: '"
                     << globalConfigPath << "'";

    store(parse_config_file(globalConfig, global), fileConfigMap);
}

std::string Options::mapEnvNames(std::string env) const
{
    boost::algorithm::to_lower(env);
    if (m_common.find_nothrow(env, false) && m_vm.count(env) == 0) {
        LOG(INFO) << "Using environment configuration variable: '" << env
                  << "'";
        return env;
    }

    return std::string();
}

void Options::parseEnv()
{
    LOG(INFO) << "Parsing environment variables";
    store(parse_environment(m_common,
              std::bind(&Options::mapEnvNames, this, std::placeholders::_1)),
        m_vm);
}

struct fuse_args Options::getFuseArgs() const
{
    struct fuse_args args = FUSE_ARGS_INIT(0, nullptr);

    fuse_opt_add_arg(&args, argv0.c_str());
    fuse_opt_add_arg(&args, "-obig_writes");

    if (m_vm.count("debug"))
        fuse_opt_add_arg(&args, "-d");
    if (m_vm.count("-f"))
        fuse_opt_add_arg(&args, "-f");
    if (m_vm.count("-s"))
        fuse_opt_add_arg(&args, "-s");

    if (m_vm.count("-o")) {
        for (const auto &opt : m_vm.at("-o").as<std::vector<std::string>>())
            fuse_opt_add_arg(&args, ("-o" + opt).c_str());
    }

    if (m_vm.count("mountpoint"))
        fuse_opt_add_arg(
            &args, m_vm.at("mountpoint").as<std::string>().c_str());

    return args;
}

std::string Options::describeCommandlineOptions() const
{
    options_description visible("");
    visible.add(m_commandline).add(m_fuse);

    std::stringstream ss;
    ss << visible;

    return ss.str();
}

} // namespace client
} // namespace one
