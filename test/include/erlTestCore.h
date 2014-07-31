/**
 * @file erlTestCore.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef ERL_TEST_CORE_H
#define ERL_TEST_CORE_H

#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <stdlib.h>

using namespace std;

#define VEILFS_ROOT_VAR             "VEILFS_ROOT"
#define COMMON_FILES_ROOT_VAR       "COMMON_FILES_ROOT"
#define TEST_ROOT_VAR               "TEST_ROOT"

namespace veil {
namespace testing {

// Executes erlang test_name:exec/1 method  
string erlExec(string arg);

// Path to dir containg all mount points
extern string VeilFSRoot;
#define MOUNT_POINT(X) veil::testing::VeilFSRoot + "/" + X

// Common files directory 
extern string CommonFilesRoot;
#define COMMON_FILE(X) veil::testing::CommonFilesRoot + "/" + X


class VeilFSMount {

public:
    VeilFSMount(string path, string cert, string opts = "", string args = "--no-check-certificate");
    ~VeilFSMount();

    string getRoot();

private:
    string m_mountPoint;

    int mount(string path, string cert, string opts, string args);
    int umount(bool silent = false);

};



} // namespace testing
} // namespace veil

using namespace veil::testing;

#endif // ERL_TEST_CORE_H