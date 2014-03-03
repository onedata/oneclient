/**
 * @file times_update_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "erlTestCore.h"
#include "boost/filesystem.hpp"
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

using namespace boost::filesystem;
using namespace std;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class TimesUpdateTest 
    : public ::testing::Test 
{
protected:
    COMMON_INTEGRATION_DEFS();

    VeilFSMount VFS;

    path directIO_root;

    TimesUpdateTest() : VFS(VeilFSMount("main", "peer.pem")) 
    {
    }

    virtual void SetUp() {
        COMMON_INTEGRATION_SETUP();

        // Get storage helper root dir path from cluster env variable 
        ASSERT_EQ(0, ::system(("touch " + VFS.getRoot() + "/file").c_str()));
    }

    virtual void TearDown() {
        ASSERT_EQ(0, ::system(("rm -rf " + VFS.getRoot() + "/file").c_str()));

        COMMON_INTEGRATION_CLEANUP();
    }

};

// Test if touch commnad updates times correctly
TEST_F(TimesUpdateTest, touchUpdate) {
    struct stat old, curr;
    sleep(2);
    stat((VFS.getRoot() + "/file").c_str(), &old);
    sleep(2);
    ASSERT_EQ(0, ::system(("touch " + VFS.getRoot() + "/file").c_str()));
    sleep(1);
    stat((VFS.getRoot() + "/file").c_str(), &curr);
    
    EXPECT_GT(curr.st_atime, old.st_atime);
    EXPECT_GT(curr.st_mtime, old.st_mtime);
    
    EXPECT_GT(curr.st_atime, (time(NULL) - 10));
    EXPECT_GT(curr.st_mtime, (time(NULL) - 10));
    
    EXPECT_LE(curr.st_atime, (time(NULL) + 10));
    EXPECT_LE(curr.st_mtime, (time(NULL) + 10));
}

// Test if data write updates times correctly
TEST_F(TimesUpdateTest, writeUpdate) {
    struct stat old, curr;
    sleep(2);
    stat((VFS.getRoot() + "/file").c_str(), &old);
    sleep(2);
    ASSERT_EQ(0, ::system(("echo 'test' > " + VFS.getRoot() + "/file").c_str()));
    sleep(1);
    stat((VFS.getRoot() + "/file").c_str(), &curr);

    EXPECT_EQ(curr.st_atime, old.st_atime);
    EXPECT_GT(curr.st_mtime, old.st_mtime);

    EXPECT_GT(curr.st_mtime, (time(NULL) - 10));

    EXPECT_LE(curr.st_mtime, (time(NULL) + 10));
}

// Test if data read commnad updates times correctly
TEST_F(TimesUpdateTest, readUpdate) {
    struct stat old, curr;
    sleep(2);
    stat((VFS.getRoot() + "/file").c_str(), &old);
    sleep(2);
    ASSERT_EQ(0, ::system(("cat " + VFS.getRoot() + "/file").c_str()));
    sleep(1);
    stat((VFS.getRoot() + "/file").c_str(), &curr);

    EXPECT_GT(curr.st_atime, old.st_atime);
    EXPECT_EQ(curr.st_mtime, old.st_mtime);

    EXPECT_GT(curr.st_atime, (time(NULL) - 10));

    EXPECT_LE(curr.st_atime, (time(NULL) + 10));
}

