/**
 * @file environment.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "environment.h"

#include "logging.h"

#include <boost/filesystem.hpp>

#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>

#include <array>
#include <cstdlib>

namespace {
boost::filesystem::path calcUserHome()
{
    if (const auto homeEnv = getenv("HOME"))
        return {homeEnv};

    return {getpwuid(getuid())->pw_dir};
}

boost::filesystem::path calcUserDataDir(const boost::filesystem::path &home)
{
    if (const auto xdgDataHomeEnv = getenv("XDG_DATA_HOME"))
        return {xdgDataHomeEnv};

    return home / ".local" / "share";
}

std::string calcClientName()
{
    std::array<char, 128> usernameBuf, hostnameBuf;

    const std::string username{
        getlogin_r(usernameBuf.data(), usernameBuf.size()) == 0
            ? usernameBuf.data()
            : "unknown"};

    const std::string hostname{
        gethostname(hostnameBuf.data(), hostnameBuf.size()) == 0
            ? hostnameBuf.data()
            : "unknown"};

    return username + '@' + hostname;
}
}

namespace one {
namespace client {

Environment::Environment()
    : m_userHome{calcUserHome()}
    , m_userDataDir{calcUserDataDir(m_userHome) / "oneclient"}
    , m_clientName{calcClientName()}
{
    if (!boost::filesystem::create_directories(m_userDataDir))
        LOG(WARNING) << "Unable to create user data directory "
                     << m_userDataDir.string();
}

const boost::filesystem::path &Environment::userDataDir() const
{
    return m_userDataDir;
}

const boost::filesystem::path &Environment::userHome() const
{
    return m_userHome;
}

const std::string &Environment::clientName() const { return m_clientName; }

} // namespace client
} // namespace one
