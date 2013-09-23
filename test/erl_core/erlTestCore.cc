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
string VeilFSRoot =  (getenv(VEILFS_ROOT_VAR) ? string(getenv(VEILFS_ROOT_VAR)) : "");
string CommonFilesRoot = (getenv(COMMON_FILES_ROOT_VAR) ? string(getenv(COMMON_FILES_ROOT_VAR)) : "");

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

} // namespace testing
} // namespace veil