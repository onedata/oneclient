/**
 * @file config.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef CONFIG_HH
#define CONFIG_HH

#include <string>
#include <yaml-cpp/yaml.h>
#include <unistd.h>
#include <map>
#include <sstream>

#include "veilConfig.h"
#include "glog/logging.h"

/// Config option names
#define CLUSTER_HOSTNAME_OPT            "cluster_hostname"
#define CLUSTER_PORT_OPT                "cluster_port"
#define LOG_DIR_OPT                     "log_dir"
#define PEER_CERTIFICATE_FILE_OPT       "peer_certificate_file"
#define ENABLE_ATTR_CACHE_OPT           "enable_attr_cache"
#define ATTR_CACHE_EXPIRATION_TIME_OPT  "attr_cache_expiration_time"
#define ENABLE_LOCATION_CACHE_OPT       "enable_location_cache"
#define ENABLE_ENV_OPTION_OVERRIDE      "enable_env_option_override"
#define FUSE_ID_OPT                     "fuse_id"

using namespace std;

/**
 * The Config singleton.
 * Parses config files and provides safe access to configuration map.
 */
class Config
{
public:
    static Config& instance();                  ///< Singleton instance
    template<typename T>
    static T getValue(string opt);              ///< Returns value of requested option.
                                                ///< You also need to provide type of returned value. Before using this function you should check is option exists, but
                                                ///< it's not required. @see Config::isSet
                                                ///< @warning If given opition wasn't set, you'll get empty object of given type T ( T() )
    string static absPathRelToCWD(string);      ///< Converts relative path, to absolute using CWD env as base prefix.
    string static absPathRelToHOME(string);     ///< Converts relative path, to absolute using HOME env as base prefix.
    bool static isSet(string);                  ///< Checks if given option is set. @see Config::getValue

    void setGlobalConfigFile(string path);      ///< Sets path to global config file. @see Config::parseConfig
    void setUserConfigFile(string path);        ///< Sets path to user config file. @see Config::parseConfig
    void setEnv();                              ///< Saves current CWD and HOME env viariables. This is required as FUSE changes them after non-debug start. This is also done automatically in Config::Config
    bool parseConfig();                         ///< Parses config from files set by Config::setGlobalConfigFile and Config::setUserConfigFile.
                                                ///< User config overides global settings.
                                                ///< If user config declares all required options, global config file isn't required, otherwise it has exists.

private:
    Config();

    static Config m_instance;                   ///< Singleton instance.
    static string m_requiredOpts[];             ///< Array containing required options names

    string m_globalConfigPath;                  ///< Path to global config file. @see Config::setGlobalConfigFile
    string m_userConfigPath;                    ///< Path to user config file. @see Config::setUserConfigFile
    string m_envCWD;                            ///< Saved CWD env variable
    string m_envHOME;                           ///< Saved HOME env variable

    YAML::Node m_globalNode;                    ///< Global config object
    YAML::Node m_userNode;                      ///< User config object
    YAML::Node m_envNode;                       ///< Temp config object used to manipulate env settings

    template<typename T>
    T get(string opt);                          ///< Internal implementation of Config::getValue. @see Config::getValue
};

template<typename T>
T Config::get(string opt)
{
    try {
        return m_userNode[opt].as<T>();
    } catch(YAML::Exception e) {
        // Just ignore and try to fetch this option form global config
    }

    try {
        return m_globalNode[opt].as<T>();
    } catch(YAML::Exception e) {
        LOG(WARNING) << "Requested option '" << opt << "' was not found in config loaded files";
        return T();
    }
}

template<typename T>
T Config::getValue(string opt)
{
    if(Config::instance().get<bool>(ENABLE_ENV_OPTION_OVERRIDE))
    {
        string upper;
        for(size_t i = 0; i < opt.size(); ++i)
            upper += toupper(opt[i]);

        char* tmp = getenv(upper.c_str());
        string env = tmp ? string(tmp) : "";

        LOG(INFO) << "Searching for " << upper << " env variable. Value: " << env;

        if(env.size() > 0)
        {
            try {
                Config::instance().m_envNode[opt] = env;
                return Config::instance().m_envNode[opt].as<T>();
            } catch(YAML::Exception e) {
                LOG(WARNING) << "ENABLE_ENV_OPTION_OVERRIDE = true but there was an error while parsing env value";
            }
        }
    }

    return Config::instance().get<T>(opt);
}

#endif // CONFIG_HH
