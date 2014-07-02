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

class TimesUpdateTest: public CommonIntegrationTest
{
protected:
    path directIO_root;

    TimesUpdateTest()
        : CommonIntegrationTest{std::unique_ptr<veil::testing::VeilFSMount>{new veil::testing::VeilFSMount{"main", "peer.pem"}}}
    {
    }

    void SetUp() override
    {
        CommonIntegrationTest::SetUp();

        // Get storage helper root dir path from cluster env variable 
        ASSERT_EQ(0, ::system(("touch " + veilFsMount->getRoot() + "/file").c_str()));
    }

    void TearDown() override {
        ASSERT_EQ(0, ::system(("rm -rf " + veilFsMount->getRoot() + "/file").c_str()));
        CommonIntegrationTest::TearDown();
    }

};

// Test if touch commnad updates times correctly
TEST_F(TimesUpdateTest, touchUpdate) {
    struct stat old, curr;
    sleep(2);
    stat((veilFsMount->getRoot() + "/file").c_str(), &old);
    sleep(2);
    ASSERT_EQ(0, ::system(("touch " + veilFsMount->getRoot() + "/file").c_str()));
    sleep(1);
    stat((veilFsMount->getRoot() + "/file").c_str(), &curr);
    
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
    stat((veilFsMount->getRoot() + "/file").c_str(), &old);
    sleep(2);
    ASSERT_EQ(0, ::system(("echo 'test' > " + veilFsMount->getRoot() + "/file").c_str()));
    sleep(1);
    stat((veilFsMount->getRoot() + "/file").c_str(), &curr);

    EXPECT_EQ(curr.st_atime, old.st_atime);
    EXPECT_GT(curr.st_mtime, old.st_mtime);

    EXPECT_GT(curr.st_mtime, (time(NULL) - 10));

    EXPECT_LE(curr.st_mtime, (time(NULL) + 10));
}

// Test if data read commnad updates times correctly
TEST_F(TimesUpdateTest, readUpdate) {
    struct stat old, curr;
    sleep(2);
    stat((veilFsMount->getRoot() + "/file").c_str(), &old);
    sleep(2);
    ASSERT_EQ(0, ::system(("cat " + veilFsMount->getRoot() + "/file").c_str()));
    sleep(1);
    stat((veilFsMount->getRoot() + "/file").c_str(), &curr);

    EXPECT_GT(curr.st_atime, old.st_atime);
    EXPECT_EQ(curr.st_mtime, old.st_mtime);

    EXPECT_GT(curr.st_atime, (time(NULL) - 10));

    EXPECT_LE(curr.st_atime, (time(NULL) + 10));
}

