/**
 * @file erlTestCore.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#include "erlTestCore.h"

#include <boost/filesystem.hpp>

namespace veil
{
namespace testing
{

// Global variables
const std::string VeilFSRoot =
        getenv(VEILFS_ROOT_VAR) ? getenv(VEILFS_ROOT_VAR) : "";
const std::string CommonFilesRoot =
        getenv(TEST_ROOT_VAR) ? std::string{getenv(TEST_ROOT_VAR)} + "/test/integration_tests/common_files" : "";

std::string erlExec(const std::string &arg)
{
    const std::string testRunner = getenv("TEST_RUNNER") ? getenv("TEST_RUNNER") : "";
    const std::string testName = getenv("TEST_NAME") ? getenv("TEST_NAME"): "";
    if(!boost::filesystem::exists(testRunner))
        return "[ERROR] Test runner not found !";

    const std::string command = testRunner + " __exec " + testName + " '" + arg + "'";
    std::string ret = "";

    FILE *out = popen(command.c_str(), "r");
    if(!out)
        return "[ERROR] popen failed !";

    char buff[1024];
    while(fgets(buff, 1024, out))
        ret += std::string(buff);

    pclose(out);
    return ret;
}

VeilFSMount::VeilFSMount(const std::string &path, const std::string &cert,
                         const std::string &opts)
{
    if(mount(path, cert, opts))
        throw std::string{"Cannot mount VFS"};
}

VeilFSMount::~VeilFSMount()
{
    umount();
}

std::string VeilFSMount::getRoot()
{
    return m_mountPoint;
}

int VeilFSMount::mount(const std::string &path, const std::string &cert,
                       const std::string &opts)
{
    m_mountPoint = MOUNT_POINT(path);
    umount(true);
    if(!boost::filesystem::create_directories(m_mountPoint))
        return -1;

    return ::system(("PEER_CERTIFICATE_FILE='" + COMMON_FILE(cert) +"' ENABLE_ATTR_CACHE='false' " +
                     opts + " veilFuse " + m_mountPoint).c_str());
}

int VeilFSMount::umount(const bool silent)
{
    boost::system::error_code ec;
    int res = ::system(("fusermount -u " + m_mountPoint + (silent ? " 2> /dev/null" : "")).c_str());
    boost::filesystem::remove_all(m_mountPoint, ec);
    return res;
}

} // namespace testing
} // namespace veil
