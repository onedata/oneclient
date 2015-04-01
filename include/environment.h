/**
 * @file environment.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_ENVIRONMENT_H
#define ONECLIENT_ENVIRONMENT_H

#include <boost/filesystem/path.hpp>

#include <string>

namespace one {
namespace client {

/**
 * @c Environment class is responsible for retrieving information about the
 * environment in which the client is running.
 */
class Environment {
public:
    /**
     * Constructor.
     * Caches information about the environment.
     */
    Environment();

    /**
     * @return Path to current user's data directory.
     */
    const boost::filesystem::path &userDataDir() const;

    /**
     * @return Path to current user's home directory.
     */
    const boost::filesystem::path &userHome() const;

    /**
     * @return Current user's client name in 'username@hostname' format.
     */
    const std::string &clientName() const;

private:
    boost::filesystem::path m_userHome;
    boost::filesystem::path m_userDataDir;
    std::string m_clientName;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_ENVIRONMENT_H
