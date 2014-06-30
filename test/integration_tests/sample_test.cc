/**
 * @file sample_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "erlTestCore.h"
#include "fslogicProxy.h"
#include "veilErrors.h"
#include "testCommon.h"

#include <boost/filesystem.hpp>

using namespace boost::filesystem;

// TEST definitions below

class SampleTest: public CommonIntegrationTest
{
protected:
    path directIO_root;

    // Mount file system in "main" subdir with "peer.pem" cert
    // use veilFsMount->getRoot() to get absolute mount point
    SampleTest()
        : CommonIntegrationTest{std::unique_ptr<veil::testing::VeilFSMount>{new veil::testing::VeilFSMount{"main", "peer.pem"}}}
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
    EXPECT_EQ("/tmp/dio", veil::testing::erlExec("{env, \"DIO_ROOT\"}"));
}

// veilFsMount->getRoot() is set to the root of mounted VeilFS. Therefore you can just
// manage some files in order to test whole VeilClient behaviourally 
TEST_F(SampleTest, mkdirExample) {
    EXPECT_EQ(0, ::system(("mkdir " + veilFsMount->getRoot() + "/testDir").c_str()));
    EXPECT_EQ(0, ::system(("rm -rf " + veilFsMount->getRoot() + "/testDir").c_str()));
}

TEST_F(SampleTest, fileExample) {
    EXPECT_EQ(0, ::system(("touch " + veilFsMount->getRoot() + "/file").c_str()));
}

TEST_F(SampleTest, fslogicExample) {
    EXPECT_EQ(VOK, fslogic->createDir("testDir", 0755));
}

