/**
 * @file sample_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "erlTestCore.h"
#include "fslogicProxy.h"
#include "oneErrors.h"
#include "testCommon.h"

#include <boost/filesystem.hpp>

using namespace boost::filesystem;

// TEST definitions below

class SampleTest: public CommonIntegrationTest
{
protected:
    path directIO_root;

    // Mount file system in "main" subdir with "peer.pem" cert
    // use onedataMount->getRoot() to get absolute mount point
    SampleTest()
        : CommonIntegrationTest{std::unique_ptr<one::testing::FsImplMount>{new one::testing::FsImplMount{"main", "peer.pem"}}}
    {
    }

    void SetUp() override
    {
        // Initialization of the whole client.
        // This initialization is not required if test uses only filesystem (theres no oneclient code calls)
        CommonIntegrationTest::SetUp();
    }
};

// This test shows how you can call sample_test:exec/1 method on cluster environment
TEST_F(SampleTest, clusterCommandExec) {
    EXPECT_EQ(std::string("/tmp/dio"), one::testing::erlExec("{env, \"DIO_ROOT\"}"));
}

// onedataMount->getRoot() is set to the root of mounted FsImpl. Therefore you can just
// manage some files in order to test whole oneclient behaviourally
TEST_F(SampleTest, mkdirExample) {
    EXPECT_EQ(0, ::system(("mkdir " + onedataMount->getRoot() + "/testDir").c_str()));
    EXPECT_EQ(0, ::system(("rm -rf " + onedataMount->getRoot() + "/testDir").c_str()));
}

TEST_F(SampleTest, fileExample) {
    EXPECT_EQ(0, ::system(("touch " + onedataMount->getRoot() + "/file").c_str()));
}

TEST_F(SampleTest, fslogicExample) {
    EXPECT_EQ(VOK, fslogic->createDir("testDir", 0755));
}

