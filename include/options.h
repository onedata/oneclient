/**
 * @file options.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_OPTIONS_H
#define ONECLIENT_OPTIONS_H

#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>
#include <fuse/fuse_opt.h>

#include <ctime>
#include <string>

/// Declare a new configuration option with a description
#define DECL_CONFIG_DESC(NAME, TYPE, DESC)                                     \
public:                                                                        \
    virtual bool has_##NAME() const                                            \
    {                                                                          \
        return m_vm.count(#NAME) && !m_vm[#NAME].defaulted();                  \
    }                                                                          \
                                                                               \
public:                                                                        \
    virtual TYPE get_##NAME() const { return m_vm.at(#NAME).as<TYPE>(); }      \
                                                                               \
private:                                                                       \
    void add_##NAME(boost::program_options::options_description &desc) const   \
    {                                                                          \
        desc.add_options()(                                                    \
            #NAME, boost::program_options::value<TYPE>(), DESC);               \
    }

/// Declare a new configuration option
#define DECL_CONFIG(NAME, TYPE) DECL_CONFIG_DESC(NAME, TYPE, "")

/// Declare a new configuration option with a default value and a description
#define DECL_CONFIG_DEF_DESC(NAME, TYPE, DEFAULT, DESC)                        \
public:                                                                        \
    virtual bool is_default_##NAME() const { return !m_vm.count(#NAME); }      \
                                                                               \
public:                                                                        \
    virtual TYPE get_##NAME() const                                            \
    {                                                                          \
        return m_vm.count(#NAME) ? m_vm.at(#NAME).as<TYPE>() : DEFAULT;        \
    }                                                                          \
                                                                               \
private:                                                                       \
    void add_##NAME(boost::program_options::options_description &desc) const   \
    {                                                                          \
        desc.add_options()(                                                    \
            #NAME, boost::program_options::value<TYPE>(), DESC);               \
    }

/// Declare a new configuration option with a default value
#define DECL_CONFIG_DEF(NAME, TYPE, DEFAULT)                                   \
    DECL_CONFIG_DEF_DESC(NAME, TYPE, DEFAULT, "")

/// Declare a command line switch (a boolean which is set to true if the switch
/// is present)
/// The description will be used in the --help.
#define DECL_CMDLINE_SWITCH_DEF(NAME, SHORT, DEFAULT, DESC)                    \
    DECL_CONFIG_DEF(NAME, bool, DEFAULT)                                       \
private:                                                                       \
    void add_switch_##NAME(boost::program_options::options_description &desc)  \
        const                                                                  \
    {                                                                          \
        desc.add_options()(#NAME SHORT, boost::program_options::value<bool>()  \
                                            ->zero_tokens()                    \
                                            ->implicit_value(true),            \
            DESC);                                                             \
    }

namespace one {

static constexpr const char *FUSE_OPT_PREFIX = "fuse_opt_";
static constexpr int ATTR_DEFAULT_EXPIRATION_TIME = 60;
static constexpr const char *GLOBAL_CONFIG_FILE = "oneclient.conf";
static constexpr const char *BASE_DOMAIN = "onedata.org";
static constexpr const char *PROVIDER_CLIENT_ENDPOINT = "/oneclient";

namespace client {

/**
* The Options class stores options set through the command line, user config
* file (passed through command line), global config file and shell environment.
* New options are introduced through DECL_* macros, which create typesafe
* get_<option_name>() functions for retrieving config settings.
*
* The option precedence is as follows (from the most important):
* - command line arguments
* - environment options (if overriding is allowed in the global config)
* - user configuration file
* - global configuration file
* - environment options (if overriding is not allowed)
*
* Only some options are available in each category. See
* Options::setDescriptions() for details.
*/
class Options {
public:
    /**
    * Constructor.
    */
    Options(boost::filesystem::path globalConfigPath);

    /**
    * Parses all available configuration sources.
    * If --help (-h) or --version (-V) options were requested, the method
    * returns immediately after recognizing them and does not parse further.
    * @param argc The number of commandline arguments passed.
    * @param argv The commandline arguments.
    */
    void parseConfigs(const int argc, const char *const argv[]);

    /**
    * Destructor.
    */
    virtual ~Options();

    /**
    * @return A populated fuse_args struct used by FUSE. Relevant options are
    * saved in the structure.
    */
    struct fuse_args getFuseArgs() const;

    /**
    * @returns A text description of commandline options.
    */
    std::string describeCommandlineOptions() const;

private:
    std::string mapEnvNames(std::string env) const;
    void setDescriptions();
    bool parseCommandLine(const int argc, const char *const argv[]);
    void parseUserConfig(boost::program_options::variables_map &fileConfigMap);
    void parseGlobalConfig(
        boost::program_options::variables_map &fileConfigMap);
    void parseEnv();

    std::string argv0;
    boost::program_options::variables_map m_vm;
    boost::program_options::options_description m_common;
    boost::program_options::options_description m_restricted;
    boost::program_options::options_description m_commandline;
    boost::program_options::options_description m_fuse;
    boost::program_options::options_description m_hidden;
    boost::filesystem::path m_globalConfigPath;

    /* clang-format off */
    DECL_CONFIG_DEF(provider_hostname, std::string, BASE_DOMAIN)
    DECL_CONFIG_DEF(provider_port, unsigned int, 5555)
    DECL_CONFIG_DEF(log_dir, std::string, "/tmp")
    DECL_CONFIG(peer_certificate_file, std::string)
    DECL_CONFIG_DEF(enable_attr_cache, bool, true)
    DECL_CONFIG_DEF(attr_cache_expiration_time, int, ATTR_DEFAULT_EXPIRATION_TIME)
    DECL_CONFIG(enable_location_cache, bool)
    DECL_CONFIG(fuse_id, std::string)
    DECL_CONFIG_DEF(cluster_ping_interval, std::time_t, 60)
    DECL_CONFIG_DEF(jobscheduler_threads, unsigned int, 3)
    DECL_CONFIG_DEF(alive_meta_connections_count, unsigned int, 2)
    DECL_CONFIG_DEF(alive_data_connections_count, unsigned int, 2)
    DECL_CONFIG_DEF(enable_dir_prefetch, bool, true)
    DECL_CONFIG_DEF(enable_parallel_getattr, bool, true)
    DECL_CONFIG_DEF(enable_permission_checking, bool, false)
    DECL_CONFIG_DEF(write_buffer_max_size, std::size_t, 64 * 1024 * 1024) // 64 MB
    DECL_CONFIG_DEF(read_buffer_max_size, std::size_t, 10 * 1024 * 1024) // 10 MB
    DECL_CONFIG_DEF(write_buffer_max_file_size, std::size_t, 64 * 1024 * 1024) // 64 MB
    DECL_CONFIG_DEF(read_buffer_max_file_size, std::size_t, 10 * 1024 * 1024) // 10 MB
    DECL_CONFIG_DEF(file_buffer_prefered_block_size, std::size_t, 100 * 1024) // 100 kB
    DECL_CONFIG_DEF(file_sync_timeout, std::time_t, 300)
    DECL_CONFIG_DEF(write_bytes_before_stat, std::size_t, 5 * 1024 * 1024) // 5 MB
    DECL_CONFIG(fuse_group_id, std::string)
    DECL_CONFIG_DEF(global_registry_url, std::string, "onedata.org")
    DECL_CONFIG_DEF(global_registry_port, unsigned int, 8443)
    DECL_CONFIG_DEF_DESC(authentication, std::string, "certificate", "authentication type to use for connection with a Provider. Accepted values are 'token' and 'certificate'.")
    DECL_CMDLINE_SWITCH_DEF(no_check_certificate, "", false, "disable remote certificate validation")
    DECL_CMDLINE_SWITCH_DEF(debug, ",d", false, "enable debug output (implies -f)")
    DECL_CMDLINE_SWITCH_DEF(debug_gsi, "", false, "enable GSI debug output")
    DECL_CMDLINE_SWITCH_DEF(help, ",h", false, "print help")
    DECL_CMDLINE_SWITCH_DEF(version, ",V", false, "print version")
    DECL_CMDLINE_SWITCH_DEF(proxyio, "", false, "force ProxyIO")
    DECL_CONFIG_DESC(config, std::string, "path to user config file")
    DECL_CONFIG_DEF(enable_env_option_override, bool, true)
    DECL_CONFIG(mountpoint, std::string)
    /* clang-format on */
};

} // namespace client
} // namespace one

#undef DECL_CONFIG
#undef DECL_CONFIG_DEF
#undef DECL_CONFIG_DESC
#undef DECL_CONFIG_DEF_DESC
#undef DECL_CMDLINE_SWITCH_DEF

#endif // ONECLIENT_OPTIONS_H
