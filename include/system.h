/**
 * @file system.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_SYSTEM_H
#define ONECLIENT_SYSTEM_H

#include <boost/filesystem/path.hpp>

#include <string>

namespace one {
namespace client {

class System {
public:
    System();

    const boost::filesystem::path &userDataDir() const;
    const boost::filesystem::path &userHome() const;
    const std::string &clientName() const;

private:
    boost::filesystem::path m_userHome;
    boost::filesystem::path m_userDataDir;
    std::string m_clientName;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_SYSTEM_H
