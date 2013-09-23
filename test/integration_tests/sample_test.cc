/**
 * @file sample_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "erlTestCore.h"
#include "boost/filesystem.hpp"

using namespace boost::filesystem;
using namespace std;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class SampleTest 
    : public ::testing::Test 
{
protected:
    COMMON_INTEGRATION_DEFS();

    path directIO_root;

    virtual void SetUp() {

        // Initialization of the whole client. 
        // This initialization is not required if test uses only filesystem (theres no VeilClient code calls)
        COMMON_INTEGRATION_SETUP();

        // Get storage helper root dir path from cluster env variable 
        directIO_root = path(erlExec("{env, \"DIO_ROOT\"}"));
        create_directory(directIO_root);
    }

    virtual void TearDown() {
        remove_all(directIO_root);

        COMMON_INTEGRATION_CLEANUP();
    }

};

// This test shows how you can call sample_test:exec/1 method on cluster environment
TEST_F(SampleTest, clusterCommandExec) {
    EXPECT_EQ(string(getenv("TEST_ROOT")) + "/directIO_root", erlExec("{env, \"DIO_ROOT\"}"));
}

// VeilFSRoot is set to the root of mounted VeilFS. Therefore you can just 
// manage some files in order to test whole VeilClient behaviourally 
TEST_F(SampleTest, mkdirExample) {
    EXPECT_EQ(0, ::system(("mkdir " + VeilFSRoot + "/testDir").c_str()));
    EXPECT_EQ(0, ::system(("rm -rf " + VeilFSRoot + "/testDir").c_str()));
}

TEST_F(SampleTest, fileExample) {
    EXPECT_EQ(0, ::system(("touch " + VeilFSRoot + "/file").c_str()));
}

TEST_F(SampleTest, fslogicExample) {
    EXPECT_EQ(VOK, fslogic->createDir("testDir", 0755));
}

