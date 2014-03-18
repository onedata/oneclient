/**
 * @file events_test.cc
 * @author Michal Sitko
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "erlTestCore.h"
#include "boost/filesystem.hpp"
#include <iostream>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

using namespace boost::filesystem;
using namespace std;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class EventsTest 
    : public ::testing::Test 
{
protected:
    COMMON_INTEGRATION_DEFS();

    boost::shared_ptr<VeilFSMount> VFS;

    path directIO_root;

    EventsTest() {}

    virtual void SetUp() {

        // Initialization of the whole client. 
        // This initialization is not required if test uses only filesystem (theres no VeilClient code calls)
        //COMMON_INTEGRATION_SETUP();
    }

    virtual void TearDown() {
        //COMMON_INTEGRATION_CLEANUP();
    }

};

string exec(const char* cmd) {
    FILE* pipe = popen(cmd, "r");
    if (!pipe) return "ERROR";
    char buffer[128];
    string result = "";
    while(!feof(pipe)) {
        if(fgets(buffer, 128, pipe) != NULL)
            result += buffer;
    }
    pclose(pipe);
    return result;
}

TEST_F(EventsTest, mkdirExample) {
    VFS.reset(new VeilFSMount("main", "peer.pem"));
    sleep(1);

    //::system(("ls -al " + VFS->getRoot() + " | wc -l").c_str());
    string res = exec(("ls -al " + VFS->getRoot() + " | wc -l").c_str());
    int before = atoi(res.c_str());
    cout << "------ IT IS BAZINGA!!:" << before << ", root: " << VFS->getRoot() << endl;

    string dirName1 = "test_dir_7";
    string dirPath1 = VFS->getRoot() + "/" + dirName1;

    EXPECT_EQ(0, ::system(("mkdir " + dirPath1).c_str()));

    res = exec(("ls -al " + VFS->getRoot() + " | wc -l").c_str());

    int after = atoi(res.c_str());
    cout << "------ IT IS BAZINGA!!after:" << after << endl;
    EXPECT_EQ(before + 1, after);

    
    erlExec("{register_mkdir_handler, \"test_user/" + dirName1 + "\"}");
    sleep(2);

    res = exec(("ls -al " + VFS->getRoot() + " | wc -l").c_str());
    before = atoi(res.c_str());
    cout << "------ IT IS BAZINGA22!!:" << before << endl;

    string dirPath2 = VFS->getRoot() + "/test_dir_8";
    EXPECT_EQ(0, ::system(("mkdir " + dirPath2).c_str()));

    sleep(2);

    res = exec(("ls -al " + VFS->getRoot() + " | wc -l").c_str());
    after = atoi(res.c_str());
    cout << "------ IT IS BAZINGA22!!after:" << after << endl;

    EXPECT_EQ(before, after);

    EXPECT_EQ(0, ::system(("rm -rf " + dirPath2).c_str()));

    //int after = atoi(::system(("ls -al " + VFS->getRoot() + " | wc -l").c_str()));
    //cout << "------ IT IS BAZINGA!!after:" << after << endl;
    //EXPECT_EQ(before + 1, after);
}

TEST_F(EventsTest, clientConfiguredAtStartup) {
    string root = MOUNT_POINT("main");
    string dirName1 = "test_dir_1";
    string dirPath1 = root + "/" + dirName1;
    string dirPath2 = root + "/test_dir_2";

    erlExec("{register_mkdir_handler, \"test_user/" + dirName1 + "\"}");
    sleep(2);

    VFS.reset(new VeilFSMount("main", "peer.pem"));
    sleep(1);

    string res = exec(("ls -al " + root + " | wc -l").c_str());
    int before = atoi(res.c_str());
    cout << "------ IT IS BAZINGA!!:" << before << ", root:" << root << endl;

    EXPECT_EQ(0, ::system(("mkdir " + dirPath1).c_str()));
    sleep(1);

    res = exec(("ls -al " + root + " | wc -l").c_str());
    int after = atoi(res.c_str());
    cout << "------ IT IS BAZINGA!!after:" << after << endl;

    EXPECT_EQ(before, after);

    res = exec(("ls -al " + root + " | wc -l").c_str());
    before = atoi(res.c_str());
    cout << "------ IT IS BAZINGA22!!:" << before << endl;

    EXPECT_EQ(0, ::system(("mkdir " + dirPath2).c_str()));
    sleep(1);

    res = exec(("ls -al " + root + " | wc -l").c_str());
    after = atoi(res.c_str());
    cout << "------ IT IS BAZINGA22!!after:" << after << endl;

    EXPECT_EQ(before + 1, after);

    EXPECT_EQ(0, ::system(("rm -rf " + dirPath2).c_str()));
}
