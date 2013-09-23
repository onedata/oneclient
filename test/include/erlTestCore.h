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

#define VEILFS_ROOT_VAR "VEILFS_ROOT"
#define COMMON_FILES_ROOT_VAR "COMMON_FILES_ROOT"

namespace veil {
namespace testing {

// Executes erlang test_name:exec/1 method  
string erlExec(string arg);

// Clients' mount point
extern string VeilFSRoot;

// Common files directory 
extern string CommonFilesRoot;
#define COMMON_FILE(X) veil::testing::CommonFilesRoot + "/" + X

} // namespace testing
} // namespace veil

using namespace veil::testing;

#endif // ERL_TEST_CORE_H