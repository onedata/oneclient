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
#include <boost/filesystem.hpp>

#include "veilConfig.h"
#include "ISchedulable.h"
#include "logging.h"
#include "lock.h"

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
#define CLUSTER_PING_INTERVAL_OPT       "cluster_ping_interval"
#define JOBSCHEDULER_THREADS_OPT        "jobscheduler_threads"
#define ALIVE_META_CONNECTIONS_COUNT_OPT     "alive_meta_connections_count"
#define ALIVE_DATA_CONNECTIONS_COUNT_OPT     "alive_data_connections_count"
#define ENABLE_DIR_PREFETCH_OPT         "enable_dir_prefetch"
#define ENABLE_PARALLEL_GETATTR_OPT     "enable_parallel_getattr"
#define WRITE_BUFFER_MAX_SIZE_OPT       "write_buffer_max_size"
#define READ_BUFFER_MAX_SIZE_OPT        "read_buffer_max_size"
#define WRITE_BUFFER_MAX_FILE_SIZE_OPT  "write_buffer_max_file_size"
#define READ_BUFFER_MAX_FILE_SIZE_OPT   "read_buffer_max_file_size"
#define FILE_BUFFER_PREFERED_BLOCK_SIZE_OPT "file_buffer_prefered_block_size"
#define FUSE_GROUP_ID_OPT               "fuse_group_id"
#define NO_CHECK_CERTIFICATE            "no_check_certificate"


/// Prefix for all env variables that will be send to cluster
#define FUSE_OPT_PREFIX               "fuse_opt_"

#define PROTOCOL_VERSION    1
#define ATTR_DEFAULT_EXPIRATION_TIME 60

#define DECLARE_DEFAULT(KEY, VALUE) m_defaultsNode[KEY] = VALUE

namespace veil {
namespace client {

namespace utils {

template<typename T>
std::string toString(T in) {
    std::ostringstream ss;
    ss << in;
    return ss.str();
}

template<typename T>
T fromString(std::string in) {
    T out = 0;
    std::istringstream iss(in);
    iss >> out;
    return out;
}

}

/**
 * The Config.
 * Parses config files and provides safe access to configuration map.
 */
class Config : public ISchedulable
{
    friend class IVeilFactory;
public:

    virtual std::string getFuseID();                            ///< Returns current FuseID.
    virtual void negotiateFuseID(time_t delay = 0);             ///< Starts FuseID negotiation process.
                                                                ///< @param delay Since this is async actions, you can specify execution delay in seconds.
    virtual void testHandshake();								///< Synchronously negotiate FuseID to test if everything is ok

    virtual std::string getString(std::string opt);             ///< Returns string value of requested option.
                                                                ///< Before using this function you should check is option exists, but
                                                                ///< it's not required. @see Config::isSet
    virtual int getInt(std::string opt);                        ///< Returns int value of requested option.
                                                                ///< Before using this function you should check is option exists, but
                                                                ///< it's not required. @see Config::isSet
    virtual double getDouble(std::string opt);                  ///< Returns double value of requested option.
                                                                ///< Before using this function you should check is option exists, but
                                                                ///< it's not required. @see Config::isSet
    virtual bool getBool(std::string opt);                      ///< Returns boolean value of requested option.
                                                                ///< Before using this function you should check is option exists, but
                                                                ///< it's not required. @see Config::isSet
                                                                ///< @warning If given opition wasn't set, you'll get empty object of given type T ( T() )

    virtual bool isSet(std::string);                            ///< Checks if given option is set. @see Config::getValue
    std::string static absPathRelToCWD(boost::filesystem::path);            ///< Converts relative path, to absolute using CWD env as base prefix.
    void static setMountPoint(boost::filesystem::path);         ///< Sets mount point path
    boost::filesystem::path static getMountPoint();             ///< Gets mount point path
    std::string static absPathRelToHOME(boost::filesystem::path);           ///< Converts relative path, to absolute using HOME env as base prefix.
    void static putEnv(std::string, std::string);               ///< Saves given env variable.

    void setGlobalConfigFile(std::string path);                 ///< Sets path to global config file. @see Config::parseConfig
    void setUserConfigFile(std::string path);                   ///< Sets path to user config file. @see Config::parseConfig
    void setEnv();                                              ///< Saves current CWD and HOME env viariables. This is required as FUSE changes them after non-debug start. This is also done automatically in Config::Config
    bool parseConfig();                                         ///< Parses config from files set by Config::setGlobalConfigFile and Config::setUserConfigFile.
                                                                ///< User config overides global settings.
                                                                ///< If user config declares all required options, global config file isn't required, otherwise it has exists.

    Config();
    virtual ~Config();

protected:
    bool defaultsLoaded;
    ReadWriteLock m_access;

    static std::string m_requiredOpts[];             ///< Array containing required options names
    static std::string m_envCWD;                     ///< Saved CWD env variable
    static std::string m_envHOME;                    ///< Saved HOME env variable
    static std::map<std::string, std::string> m_envAll; ///< All saved env variables
    static boost::filesystem::path m_mountPoint;

    std::string m_globalConfigPath;                  ///< Path to global config file. @see Config::setGlobalConfigFile
    std::string m_userConfigPath;                    ///< Path to user config file. @see Config::setUserConfigFile

    YAML::Node m_globalNode;                         ///< Global config object
    YAML::Node m_userNode;                           ///< User config object
    YAML::Node m_envNode;                            ///< Temp config object used to manipulate env settings
    YAML::Node m_defaultsNode;                       ///< Default configs.

    template<typename T>
    T get(std::string opt);                          ///< Internal implementation of Config::getValue. @see Config::getValue

    template<typename T>
    T getValue(std::string opt);                     ///< Returns type-specialized value of given config option.

    std::string static absPathRelTo(boost::filesystem::path relTo, boost::filesystem::path p); ///< Converts relative path (second argument), to absolute (relative to first argument). Also preforms check against mount point.


    virtual bool runTask(TaskID taskId, std::string arg0, std::string arg1, std::string arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

    void setupDefaults() {
        /// Here declare default values of config
        DECLARE_DEFAULT(ENABLE_ENV_OPTION_OVERRIDE, true);
        DECLARE_DEFAULT(CLUSTER_PORT_OPT, 5555);
        DECLARE_DEFAULT(CLUSTER_PING_INTERVAL_OPT, 60);
        DECLARE_DEFAULT(JOBSCHEDULER_THREADS_OPT, 3);
        DECLARE_DEFAULT(ATTR_CACHE_EXPIRATION_TIME_OPT, ATTR_DEFAULT_EXPIRATION_TIME);
        DECLARE_DEFAULT(ENABLE_ATTR_CACHE_OPT, true);
        DECLARE_DEFAULT(ALIVE_META_CONNECTIONS_COUNT_OPT, 2);
        DECLARE_DEFAULT(ALIVE_DATA_CONNECTIONS_COUNT_OPT, 2);
        DECLARE_DEFAULT(ENABLE_PARALLEL_GETATTR_OPT, true);
        DECLARE_DEFAULT(ENABLE_DIR_PREFETCH_OPT, true);
        DECLARE_DEFAULT(LOG_DIR_OPT, "/tmp");
        DECLARE_DEFAULT(NO_CHECK_CERTIFICATE, true);

        DECLARE_DEFAULT(WRITE_BUFFER_MAX_SIZE_OPT, 64 * 1024 * 1024);       // 64 MB
        DECLARE_DEFAULT(READ_BUFFER_MAX_SIZE_OPT, 10 * 1024 * 1024);        // 10 MB
        DECLARE_DEFAULT(WRITE_BUFFER_MAX_FILE_SIZE_OPT, 64 * 1024 * 1024);  // 64 MB
        DECLARE_DEFAULT(READ_BUFFER_MAX_FILE_SIZE_OPT, 10 * 1024 * 1024);   // 10 MB
        DECLARE_DEFAULT(FILE_BUFFER_PREFERED_BLOCK_SIZE_OPT, 100 * 1024);   // 100 kB
    }

};

template<typename T>
T Config::get(std::string opt)
{
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
        // Just ignore and try to fetch this option form default values
    }

    try {
        return m_defaultsNode[opt].as<T>();
    } catch(YAML::Exception e) {
        LOG(WARNING) << "Requested option '" << opt << "' was not found in app config";
        return T();
    }
}

template<typename T>
T Config::getValue(std::string opt)
{
    if(get<bool>(ENABLE_ENV_OPTION_OVERRIDE))
    {
        std::string upper;
        for(size_t i = 0; i < opt.size(); ++i)
            upper += toupper(opt[i]);

        char* tmp = getenv(upper.c_str());
        std::string env = tmp ? std::string(tmp) : "";

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

} // namespace client
} // namespace veil

#endif // CONFIG_HH
