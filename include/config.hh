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

#define DECLARE_DEFAULT(KEY, VALUE) m_defaultsNode[KEY] = VALUE

using namespace std;


/**
 * The Config.
 * Parses config files and provides safe access to configuration map.
 */
class Config
{
    friend class IVeilFactory;
public:
    virtual string getString(string opt);       ///< Returns string value of requested option.
                                                ///< Before using this function you should check is option exists, but
                                                ///< it's not required. @see Config::isSet
    virtual int getInt(string opt);             ///< Returns int value of requested option.
                                                ///< Before using this function you should check is option exists, but
                                                ///< it's not required. @see Config::isSet
    virtual double getDouble(string opt);               ///< Returns double value of requested option.
                                                ///< Before using this function you should check is option exists, but
                                                ///< it's not required. @see Config::isSet
    virtual bool getBool(string opt);           ///< Returns boolean value of requested option.
                                                ///< Before using this function you should check is option exists, but
                                                ///< it's not required. @see Config::isSet
                                                ///< @warning If given opition wasn't set, you'll get empty object of given type T ( T() )
    
    virtual bool isSet(string);                 ///< Checks if given option is set. @see Config::getValue

    string static absPathRelToCWD(string);      ///< Converts relative path, to absolute using CWD env as base prefix.
    string static absPathRelToHOME(string);     ///< Converts relative path, to absolute using HOME env as base prefix.
    
    void setGlobalConfigFile(string path);      ///< Sets path to global config file. @see Config::parseConfig
    void setUserConfigFile(string path);        ///< Sets path to user config file. @see Config::parseConfig
    void setEnv();                              ///< Saves current CWD and HOME env viariables. This is required as FUSE changes them after non-debug start. This is also done automatically in Config::Config
    bool parseConfig();                         ///< Parses config from files set by Config::setGlobalConfigFile and Config::setUserConfigFile.
                                                ///< User config overides global settings.
                                                ///< If user config declares all required options, global config file isn't required, otherwise it has exists.

    Config();
    virtual ~Config();

private:
    static string m_requiredOpts[];             ///< Array containing required options names
    static string m_envCWD;                     ///< Saved CWD env variable
    static string m_envHOME;                    ///< Saved HOME env variable

    string m_globalConfigPath;                  ///< Path to global config file. @see Config::setGlobalConfigFile
    string m_userConfigPath;                    ///< Path to user config file. @see Config::setUserConfigFile

    YAML::Node m_globalNode;                    ///< Global config object
    YAML::Node m_userNode;                      ///< User config object
    YAML::Node m_envNode;                       ///< Temp config object used to manipulate env settings
    YAML::Node m_defaultsNode;                  ///< Default configs.

    template<typename T>
    T get(string opt);                          ///< Internal implementation of Config::getValue. @see Config::getValue
    
    template<typename T>
    T getValue(string opt);                     ///< Returns type-specialized value of given config option. 

    void setupDefaults() {
        /// Here declare default values of config
        DECLARE_DEFAULT(ENABLE_ENV_OPTION_OVERRIDE, true);
    }
};

template<typename T>
T Config::get(string opt)
{
    static bool defaultsLoaded = false; 
    // NOTICE: 
    //     Because this is template method, this initialization (setupDefaults) can and will run several times
    //     but since it has no other side effects in any subsequent calls, we can live with that.
    if(!defaultsLoaded) {
        setupDefaults();
        defaultsLoaded = true;
    }
        
    try {
        return m_userNode[opt].as<T>();
    } catch(YAML::Exception e) {
        // Just ignore and try to fetch this option form global config
    }

    try {
        return m_globalNode[opt].as<T>();
    } catch(YAML::Exception e) {
        LOG(WARNING) << "Requested option '" << opt << "' was not found in config loaded files";
    }

    try {
        return m_defaultsNode[opt].as<T>();
    } catch(YAML::Exception e) {
        return T();
    }
}

template<typename T>
T Config::getValue(string opt)
{
    if(get<bool>(ENABLE_ENV_OPTION_OVERRIDE))
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
                m_envNode[opt] = env;
                return m_envNode[opt].as<T>();
            } catch(YAML::Exception e) {
                LOG(WARNING) << "ENABLE_ENV_OPTION_OVERRIDE = true but there was an error while parsing env value";
            }
        }
    }

    return get<T>(opt);
}

#endif // CONFIG_HH
