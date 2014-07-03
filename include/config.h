/**
 * @file config.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILCLIENT_CONFIG_H
#define VEILCLIENT_CONFIG_H


#include "ISchedulable.h"
#include "lock.h"

#include <boost/filesystem.hpp>

#include <map>
#include <memory>
#include <sstream>
#include <string>

namespace veil
{

/// Prefix for all env variables that will be send to cluster
static constexpr const char *FUSE_OPT_PREFIX = "fuse_opt_";
static constexpr int ATTR_DEFAULT_EXPIRATION_TIME = 60;

namespace client
{

class Context;

namespace utils
{

template<typename T>
std::string toString(const T &in)
{
    std::ostringstream ss;
    ss << in;
    return ss.str();
}

template<typename T>
T fromString(const std::string &in)
{
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
class Config: public ISchedulable
{
public:
    virtual std::string getFuseID();                             ///< Returns current FuseID.
    virtual void negotiateFuseID(time_t delay = 0);              ///< Starts FuseID negotiation process.
                                                                 ///< @param delay Since this is async actions, you can specify execution delay in seconds.
    virtual void testHandshake();                                ///< Synchronously negotiate FuseID to test if everything is ok


    std::string absPathRelToCWD(const boost::filesystem::path&); ///< Converts relative path, to absolute using CWD env as base prefix.
    void setMountPoint(boost::filesystem::path);                 ///< Sets mount point path
    boost::filesystem::path getMountPoint();                     ///< Gets mount point path
    std::string absPathRelToHOME(const boost::filesystem::path&);///< Converts relative path, to absolute using HOME env as base prefix.

    void setEnv();                                               ///< Saves current CWD and HOME env viariables. This is required as FUSE changes them after non-debug start. This is also done automatically in Config::Config
    void putEnv(std::string name, std::string value);
    bool isEnvSet(const std::string&);                           ///< Checks whether env variable is set.

    Config(std::weak_ptr<Context> context);
    virtual ~Config();

protected:
    ReadWriteLock m_access;

    std::string m_envCWD;                     ///< Saved CWD env variable
    std::string m_envHOME;                    ///< Saved HOME env variable
    std::map<std::string, std::string> m_envAll; ///< All saved env variables
    boost::filesystem::path m_mountPoint;

    std::string m_fuseID;

    std::string absPathRelTo(const boost::filesystem::path &relTo, boost::filesystem::path p); ///< Converts relative path (second argument), to absolute (relative to first argument). Also preforms check against mount point.

    virtual bool runTask(TaskID taskId, const std::string &arg0, const std::string &arg1, const std::string &arg3); ///< Task runner derived from ISchedulable. @see ISchedulable::runTask

private:
    const std::weak_ptr<Context> m_context;
};

} // namespace client
} // namespace veil


#endif // VEILCLIENT_CONFIG_H
