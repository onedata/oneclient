/**
 * @file events_test.cc
 * @author Michal Sitko
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "erlTestCore.h"
#include "boost/filesystem.hpp"
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

using namespace boost::filesystem;
using namespace veil;

class EventsTest: public CommonIntegrationTest
{
protected:
    path directIO_root;

    // VFS is not initialized here because in some test cases we want to perform
    // some actions on cluster before client initialization
    EventsTest()
        : CommonIntegrationTest{{}}
    {
    }
};

std::string exec(const char* cmd)
{
    FILE* pipe = popen(cmd, "r");
    if (!pipe) return "ERROR";
    char buffer[128];
    std::string result = "";
    while(!feof(pipe)) {
        if(fgets(buffer, 128, pipe) != NULL)
            result += buffer;
    }
    pclose(pipe);
    return result;
}

// In this test we perform some action with and without event handler registered
TEST_F(EventsTest, mkdirExample)
{
    // given
    veilFsMount.reset(new veil::testing::VeilFSMount("main", "peer.pem"));
    sleep(2);
    std::string dirName1 = "test_dir_7";
    std::string dirPath1 = veilFsMount->getRoot() + "/" + dirName1;
    std::string dirPath2 = veilFsMount->getRoot() + "/test_dir_8";

    std::string res = exec(("ls -al " + veilFsMount->getRoot() + " | wc -l").c_str());
    int before = atoi(res.c_str());

    // what
    EXPECT_EQ(0, ::system(("mkdir " + dirPath1).c_str()));
    sleep(3);

    // then
    res = exec(("ls -al " + veilFsMount->getRoot() + " | wc -l").c_str());
    int after = atoi(res.c_str());

    // no event handler was registered so number of files after should be equal before + 1
    EXPECT_EQ(before + 1, after);

    // event handler registration, directory dirName1 will be deleted on directory creation
    veil::testing::erlExec("{register_mkdir_handler, \"test_user/" + dirName1 + "\"}");

    // given
    res = exec(("ls -al " + veilFsMount->getRoot() + " | wc -l").c_str());
    before = atoi(res.c_str());

    // what
    EXPECT_EQ(0, ::system(("mkdir " + dirPath2).c_str()));
    sleep(3);

    // then
    res = exec(("ls -al " + veilFsMount->getRoot() + " | wc -l").c_str());
    after = atoi(res.c_str());

    // this time we expect that dirPath2 has been created, event handler applied which deleted dirPath1, so we expect before == after
    EXPECT_EQ(before, after);

    EXPECT_EQ(0, ::system(("rm -rf " + dirPath2).c_str()));
}

// Checks if client get event producer configuration on startup
TEST_F(EventsTest, clientConfiguredAtStartup)
{
    std::string root = veil::testing::MOUNT_POINT("main");
    std::string dirName1 = "test_dir_1";
    std::string dirPath1 = root + "/" + dirName1;
    std::string dirPath2 = root + "/test_dir_2";

    // this is essential for this test to register event handler before mounting and initializing client
    veil::testing::erlExec("{register_mkdir_handler, \"test_user/" + dirName1 + "\"}");
    sleep(1);

    veilFsMount.reset(new veil::testing::VeilFSMount("main", "peer.pem"));
    sleep(2);

    // given
    std::string res = exec(("ls -al " + root + " | wc -l").c_str());
    int before = atoi(res.c_str());

    //what
    EXPECT_EQ(0, ::system(("mkdir " + dirPath1).c_str()));
    sleep(3);

    // then
    res = exec(("ls -al " + root + " | wc -l").c_str());
    int after = atoi(res.c_str());

    EXPECT_EQ(before, after);

    // given
    res = exec(("ls -al " + root + " | wc -l").c_str());
    before = atoi(res.c_str());

    // what
    EXPECT_EQ(0, ::system(("mkdir " + dirPath2).c_str()));
    sleep(3);

    // then
    res = exec(("ls -al " + root + " | wc -l").c_str());
    after = atoi(res.c_str());

    EXPECT_EQ(before + 1, after);

    EXPECT_EQ(0, ::system(("rm -rf " + dirPath2).c_str()));
}

TEST_F(EventsTest, clientGettingBlockedWhenQuotaExceeded)
{
    veilFsMount.reset(new veil::testing::VeilFSMount("main", "peer.pem"));
    sleep(1);

    std::string root = veil::testing::MOUNT_POINT("main");
    std::string filePath = root + "/quota_test_file";
    std::string filePath2 = root + "/quota_test_file2";
    std::string filePath3 = root + "/quota_test_file3";
    std::string filePath4 = root + "/quota_test_file4";
    EXPECT_EQ(0, ::system(("touch " + filePath).c_str()));
    EXPECT_EQ(0, ::system(("touch " + filePath2).c_str()));

    veil::testing::erlExec("{prepare_for_quota_case, 100}");

    // write 100 bytes
    for(int i=0; i<10; ++i){
        EXPECT_EQ(0, ::system(("dd if=/dev/zero bs=1 count=10 >> " + filePath).c_str()));
    }

    // it may be enough to call dd just once but quota view results from db may be stale.
    // after 3 calls it has to be recent enough to trigger quota exceeded event
    for(int i=0; i<3; ++i){
        ::system(("dd if=/dev/zero bs=1 count=10 >> " + filePath).c_str());
        sleep(1);
    }

    // trying to write something should return error
    EXPECT_TRUE(::system(("dd if=/dev/zero bs=1 count=10 >> " + filePath).c_str()) != 0);

    // we are deleting big file - after that we should fits our quota again
    EXPECT_EQ(0, ::system(("rm " + filePath).c_str()));

    // calling dd two times to trigger events handlers and db views update
    ::system(("dd if=/dev/zero bs=1 count=10 >> " + filePath2).c_str());
    ::system(("dd if=/dev/zero bs=1 count=10 >> " + filePath2).c_str());

    // now we should be able to write again
    EXPECT_EQ(0, ::system(("dd if=/dev/zero bs=1 count=10 >> " + filePath2).c_str()));

}
