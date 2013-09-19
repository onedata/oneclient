/**
 * @file sample_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.hh"
#include "erlTestCore.hh"
#include "boost/filesystem.hpp"

#include "veilfs.hh"

using namespace boost::filesystem;
using namespace std;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class SampleTest 
    : public ::testing::Test 
{
protected:

    shared_ptr<VeilFS> veilFS;
    shared_ptr<FslogicProxy> fslogic;

    path directIO_root;

    virtual void SetUp() {
        fslogic.reset(new FslogicProxy());

        // Initialization of the whole client. Note that we are using real objects, not mocks
        // since we want "integration test"
        // Also note that we don't run Config.parseConfig. It would fail since we don't have config file
        // and we don't it one. All required configs are overriden by env variables
        // set by CI.
        // This initialization is not required if test uses only filesystem (theres no VeilClient code calls)
        veilFS.reset(new VeilFS(VeilFSRoot, shared_ptr<Config>(new Config()), 
                        shared_ptr<JobScheduler>(new JobScheduler()), 
                        shared_ptr<FslogicProxy>(fslogic), 
                        shared_ptr<MetaCache>(new MetaCache()), 
                        shared_ptr<StorageMapper>(new StorageMapper(*new FslogicProxy())),
                        shared_ptr<StorageHelperFactory>(new StorageHelperFactory())));

        directIO_root = path(erlExec("{env, \"DIO_ROOT\"}"));
        create_directory(directIO_root);
    }

    virtual void TearDown() {
        remove_all(directIO_root);
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

