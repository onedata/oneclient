/**
 * @file options.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "options.h"
#include "option.h"
#include "optionsParser.h"
#include "version.h"

#include <boost/program_options.hpp>

#include <iostream>
#include <sstream>
#include <vector>

namespace one {
namespace client {
namespace options {

Options::Options()
    : m_defaultConfigFilePath{boost::filesystem::path(ONECLIENT_CONFIG_DIR) /
          boost::filesystem::path(CONFIG_FILE_NAME)}
    , m_defaultLogDirPath{"/tmp/oneclient/" + std::to_string(geteuid())}
{
    add<bool>()
        ->asSwitch()
        .withShortName("h")
        .withLongName("help")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Show this help and exit.");

    add<bool>()
        ->asSwitch()
        .withShortName("V")
        .withLongName("version")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Show current Oneclient version and exit.");

    add<bool>()
        ->asSwitch()
        .withShortName("u")
        .withLongName("unmount")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Unmount Oneclient and exit.");

    add<boost::filesystem::path>()
        ->withShortName("c")
        .withLongName("config")
        .withEnvName("config")
        .withValueName("<path>")
        .withDefaultValue(
            m_defaultConfigFilePath, m_defaultConfigFilePath.c_str())
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify path to config file.");

    add<std::string>()
        ->withShortName("H")
        .withLongName("host")
        .withConfigName("provider_host")
        .withEnvName("provider_host")
        .withValueName("<host>")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify the hostname of the Oneprovider instance to "
                         "which the Oneclient should connect.");

    add<unsigned int>()
        ->withShortName("P")
        .withLongName("port")
        .withConfigName("provider_port")
        .withEnvName("provider_port")
        .withValueName("<port>")
        .withDefaultValue(
            DEFAULT_PROVIDER_PORT, std::to_string(DEFAULT_PROVIDER_PORT))
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify the port to which the Oneclient should "
                         "connect on the Oneprovider.");

    add<bool>()
        ->asSwitch()
        .withShortName("i")
        .withLongName("insecure")
        .withConfigName("insecure")
        .withEnvName("insecure")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Disable verification of server certificate, allows "
                         "to connect to servers without valid certificate.");

    add<std::string>()
        ->withShortName("t")
        .withLongName("token")
        .withConfigName("access_token")
        .withEnvName("access_token")
        .withValueName("<token>")
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify Onedata access token for authentication and "
                         "authorization.");

    add<boost::filesystem::path>()
        ->withShortName("l")
        .withLongName("log-dir")
        .withConfigName("log_dir")
        .withEnvName("log_dir")
        .withValueName("<path>")
        .withDefaultValue(m_defaultLogDirPath, m_defaultLogDirPath.c_str())
        .withGroup(OptionGroup::GENERAL)
        .withDescription("Specify custom path for Oneclient logs.");

    add<bool>()
        ->asSwitch()
        .withShortName("f")
        .withLongName("foreground")
        .withConfigName("fuse_foreground")
        .withEnvName("fuse_foreground")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::FUSE)
        .withDescription("Foreground operation.");

    add<bool>()
        ->asSwitch()
        .withShortName("d")
        .withLongName("debug")
        .withConfigName("fuse_debug")
        .withEnvName("fuse_debug")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::FUSE)
        .withDescription("Enable debug mode (implies -f).");

    add<bool>()
        ->asSwitch()
        .withShortName("s")
        .withLongName("single-thread")
        .withConfigName("fuse_single_thread")
        .withEnvName("fuse_single_thread")
        .withImplicitValue(true)
        .withDefaultValue(false, "false")
        .withGroup(OptionGroup::FUSE)
        .withDescription("Single-threaded operation.");

    add<std::vector<std::string>>()
        ->withShortName("o")
        .withLongName("opt")
        .withConfigName("fuse_mount_opt")
        .withValueName("<mount_option>")
        .withGroup(OptionGroup::FUSE)
        .withDescription("Pass mount arguments directly to FUSE.");

    add<boost::filesystem::path>()
        ->required()
        .positional("mountpoint", 1)
        .withConfigName("mountpoint")
        .withEnvName("mountpoint")
        .withValueName("<path>")
        .withGroup(OptionGroup::INVISIBLE)
        .withDescription("Specify path for Oneclient mountpoint.");

    add<std::string>()
        ->withEnvName("authorization_token")
        .withGroup(OptionGroup::DEPRECATED);

    add<std::string>()
        ->withLongName("authentication")
        .withConfigName("authentication")
        .withGroup(OptionGroup::DEPRECATED);

    add<bool>()
        ->asSwitch()
        .withLongName("no_check_certificate")
        .withEnvName("no_check_certificate")
        .withConfigName("no_check_certificate")
        .withGroup(OptionGroup::DEPRECATED);
}

void Options::parse(const int argc, const char *const argv[])
{
    if (argc == 1) {
        m_emptyArgumentsList = true;
        return;
    }

    OptionsParser parser{m_options};
    parser.parseCommandLine(argc, argv, m_vm);

    if (getHelp() || getVersion())
        return;

    parser.parseEnvironment(m_deprecatedEnvs, m_vm);
    parser.parseConfigFile(getConfigFilePath(), m_vm);
    boost::program_options::notify(m_vm);
}

std::string Options::formatDeprecated() const
{
    std::stringstream ss;
    for (const auto &env : m_deprecatedEnvs) {
        ss << "WARNING: use of environment variable '" << env << "' without '"
           << ENVIRONMENT_PREFIX2
           << "' prefix is deprecated and will be removed in the future.\n";
    }
    for (const auto &name : selectDeprecated()) {
        ss << "WARNING: option '" << name
           << "' is deprecated and will be removed in the future.\n";
    }
    return ss.str();
}

std::string Options::formatHelp(const char *programName) const
{
    boost::program_options::options_description general{"General options"};
    selectCommandLine(general, OptionGroup::GENERAL);
    boost::program_options::options_description fuse{"FUSE options"};
    selectCommandLine(fuse, OptionGroup::FUSE);

    std::stringstream ss;
    ss << "Usage: " << programName << " [options] mountpoint\n";
    ss << "\n";
    ss << "A Onedata command line client.";
    ss << "\n";
    ss << "\n";
    ss << general;
    ss << "\n";
    ss << fuse;

    return ss.str();
}

bool Options::hasDeprecated()
{
    return !m_deprecatedEnvs.empty() || !selectDeprecated().empty();
}

bool Options::getHelp() const
{
    return m_emptyArgumentsList || get<bool>({"help"}).get_value_or(false);
}

bool Options::getVersion() const
{
    return get<bool>({"version"}).get_value_or(false);
}

bool Options::getUnmount() const
{
    return get<bool>({"unmount"}).get_value_or(false);
}

bool Options::getForeground() const
{
    return get<bool>({"foreground", "fuse_foreground"}).get_value_or(false);
}

bool Options::getDebug() const
{
    return get<bool>({"debug", "fuse_debug"}).get_value_or(false);
}

bool Options::getSingleThread() const
{
    return get<bool>({"single-thread", "fuse_single_thread"})
        .get_value_or(false);
}

boost::optional<std::string> Options::getProviderHost() const
{
    return get<std::string>({"host", "provider_host"});
}

unsigned int Options::getProviderPort() const
{
    return get<unsigned int>({"port", "provider_port"})
        .get_value_or(DEFAULT_PROVIDER_PORT);
}

bool Options::getInsecure() const
{
    return get<bool>(
        {"insecure", "no-check-certificate", "no_check_certificate"})
        .get_value_or(false);
}

boost::optional<std::string> Options::getAccessToken() const
{
    return get<std::string>({"token", "access_token", "authorization_token"});
}

boost::filesystem::path Options::getConfigFilePath() const
{
    return get<boost::filesystem::path>({"config"})
        .get_value_or(m_defaultConfigFilePath);
}

boost::filesystem::path Options::getLogDirPath() const
{
    return get<boost::filesystem::path>({"log-dir", "log_dir"})
        .get_value_or(m_defaultLogDirPath);
}

boost::filesystem::path Options::getMountpoint() const
{
    return get<boost::filesystem::path>({"mountpoint"}).get();
}

std::vector<std::string> Options::getFuseOpts() const
{
    return get<std::vector<std::string>>({"opt", "fuse_mount_opt"})
        .get_value_or({});
}

struct fuse_args Options::getFuseArgs(const char *programName) const
{
    struct fuse_args args = FUSE_ARGS_INIT(0, nullptr);

    fuse_opt_add_arg(&args, programName);
    fuse_opt_add_arg(&args, "-obig_writes");

    if (getDebug())
        fuse_opt_add_arg(&args, "-d");
    if (getForeground())
        fuse_opt_add_arg(&args, "-f");
    if (getSingleThread())
        fuse_opt_add_arg(&args, "-s");

    for (const auto &opt : getFuseOpts())
        fuse_opt_add_arg(&args, ("-o" + opt).c_str());

    fuse_opt_add_arg(&args, getMountpoint().c_str());

    return args;
}

void Options::selectCommandLine(
    boost::program_options::options_description &desc,
    const OptionGroup &group) const
{
    for (const auto &option : m_options) {
        if (option->hasGroup(group))
            option->addCommandLine(desc);
    }
}

std::vector<std::string> Options::selectDeprecated() const
{
    std::vector<std::string> deprecated;
    for (const auto &option : m_options) {
        if (option->hasGroup(OptionGroup::DEPRECATED) && option->isSet(m_vm))
            deprecated.push_back(option->name());
    }
    return deprecated;
}

} // namespace options
} // namespace client
} // namespace one
