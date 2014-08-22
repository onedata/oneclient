/**
 * @file erlTestCore.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#include "erlTestCore.h"
#include <boost/filesystem.hpp>

using namespace std;
using namespace boost;


namespace veil {
namespace testing { 

// Global variables
string VeilFSRoot =             (getenv(VEILFS_ROOT_VAR) ? string(getenv(VEILFS_ROOT_VAR)) : "");
string CommonFilesRoot =        (getenv(TEST_ROOT_VAR) ? (string(getenv(TEST_ROOT_VAR)) + "/test/integration_tests/common_files") : "");

string erlExec(string arg) {
    string testRunner = getenv("TEST_RUNNER") ? string(getenv("TEST_RUNNER")) : "";
    string testName = getenv("TEST_NAME") ? string(getenv("TEST_NAME")): "";
    if(!filesystem::exists(testRunner))
        return "[ERROR] Test runner not found !";

    string command = testRunner + " __exec " + testName + " '" + arg + "'";
    string ret = "";

    FILE *out = popen(command.c_str(), "r");
    if(!out)
        return "[ERROR] popen failed !";
    
    char buff[1024];
    while(fgets(buff, 1024, out)) 
        ret += string(buff);
        
    pclose(out);
    return ret;
}

VeilFSMount::VeilFSMount(string path, string cert, string opts, string args)
{ 
    if(mount(path, cert, opts, args))
        throw string("Cannot mount VFS");
}

VeilFSMount::~VeilFSMount() {
    umount();
}

string VeilFSMount::getRoot() {
    return m_mountPoint;
}

int VeilFSMount::mount(string path, string cert, string opts, string args) {
    m_mountPoint = MOUNT_POINT(path);
    (void) umount(true);
    if(!filesystem::create_directories(m_mountPoint))
        return -1;

    return ::system(("PEER_CERTIFICATE_FILE='" + COMMON_FILE(cert) + "' ENABLE_ATTR_CACHE='false' " + 
                     opts + " veilFuse " + args + " " + m_mountPoint).c_str());
}

int VeilFSMount::umount(bool silent) {
    boost::system::error_code ec;
    int res = ::system(("fusermount -u " + m_mountPoint + (silent ? " 2> /dev/null" : "")).c_str());
    (void) filesystem::remove_all(m_mountPoint, ec);
    return res;
}

} // namespace testing
} // namespace veil