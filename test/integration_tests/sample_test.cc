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

// TEST definitions below

class SampleTest: CommonIntegrationTest
{
protected:
    path directIO_root;

    // Mount file system in "main" subdir with "peer.pem" cert
    // use VFS.getRoot() to get absolute mount point
    SampleTest()
        : CommonIntegrationTest{{"main", "peer.pem"}}
    {
    }

    void SetUp() override
    {
        // Initialization of the whole client. 
        // This initialization is not required if test uses only filesystem (theres no VeilClient code calls)
        CommonIntegrationTest::SetUp();
    }
};

// This test shows how you can call sample_test:exec/1 method on cluster environment
TEST_F(SampleTest, clusterCommandExec) {
    EXPECT_EQ(string("/tmp/dio"), erlExec("{env, \"DIO_ROOT\"}"));
}

// VFS.getRoot() is set to the root of mounted VeilFS. Therefore you can just 
// manage some files in order to test whole VeilClient behaviourally 
TEST_F(SampleTest, mkdirExample) {
    EXPECT_EQ(0, ::system(("mkdir " + VFS.getRoot() + "/testDir").c_str()));
    EXPECT_EQ(0, ::system(("rm -rf " + VFS.getRoot() + "/testDir").c_str()));
}

TEST_F(SampleTest, fileExample) {
    EXPECT_EQ(0, ::system(("touch " + VFS.getRoot() + "/file").c_str()));
}

TEST_F(SampleTest, fslogicExample) {
    EXPECT_EQ(VOK, fslogic->createDir("testDir", 0755));
}

