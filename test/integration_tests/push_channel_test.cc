/**
 * @file push_channel_test.cc
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
#include <boost/thread/thread_time.hpp>

using namespace boost::filesystem;
using namespace std;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;
using namespace veil::client::utils;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class PushChannelTest
: public ::testing::Test
{
protected:
    COMMON_INTEGRATION_DEFS();
    
    VeilFSMount VFS;
    
    path directIO_root;
    
    boost::mutex cbMutex;
    boost::condition cbCond;
    int answerHandled;
    
    PushChannelTest() : VFS(VeilFSMount("main", "peer.pem"))
    {
    }
    
    virtual void SetUp() {
        COMMON_INTEGRATION_SETUP();
        answerHandled = 0;
    }
    
    virtual void TearDown() {
        COMMON_INTEGRATION_CLEANUP();
    }
    
public:
    
    bool handler(const protocol::communication_protocol::Answer &msg, int waitFor)
    {
        boost::unique_lock<boost::mutex> lock(cbMutex);
        TestChannelAnswer tMsg;
        
        tMsg.ParseFromString(msg.worker_answer());
        if(tMsg.message() == "test")
            answerHandled++;

        if(answerHandled >= waitFor)
            cbCond.notify_all();
        
        return true;
    }
    
};

// Test if PUSH channel registration and close works well
TEST_F(PushChannelTest, RegisterAndClose) {
    // By default client should register at least one handler
    
    ASSERT_LT(0, fromString<int>(erlExec(string("{get_handler_count, \"") + config->getFuseID() + string("\"}"))));
    
    // Make sure we have only one connection
    context->getConnectionPool()->setPoolSize(SimpleConnectionPool::META_POOL, 1);
    
    // Close PUSH channel
    context->getConnectionPool()->selectConnection()->disablePushChannel();
    sleep(2);
    ASSERT_EQ(0, fromString<int>(erlExec(string("{get_handler_count, \"") + config->getFuseID() + string("\"}"))));
}


// Test if PUSH channel failure doesnt break its PUSH handler status
TEST_F(PushChannelTest, pushChannelFailure) {
    // Make sure we have only one connection
    context->getConnectionPool()->setPoolSize(SimpleConnectionPool::META_POOL, 1);
    
    // Close PUSH channel
    boost::shared_ptr<CommunicationHandler> conn = context->getConnectionPool()->selectConnection();
    
    conn->closeConnection();
    int connectRes = conn->openConnection();
    ASSERT_EQ(0, connectRes);
    sleep(2);
    
    ASSERT_LT(0, fromString<int>(erlExec(string("{get_handler_count, \"") + config->getFuseID() + string("\"}"))));
}


// Test if PUSH channel receives messages
TEST_F(PushChannelTest, pushChannelInbox) {
    boost::unique_lock<boost::mutex> lock(cbMutex);
    
    // Register handler
    VeilFS::getPushListener()->subscribe(boost::bind(&PushChannelTest::handler, this, _1, 2));
    
    // Send test message from cluster
    string sendAns = erlExec(string("{push_msg, \"test\", \"") + config->getFuseID() + "\"}");
    EXPECT_EQ("ok", sendAns);
    
    sendAns = erlExec(string("{push_msg, \"test\", \"") + config->getFuseID() + "\"}");
    EXPECT_EQ("ok", sendAns);
    
    // Timeout after 5 secs
    cbCond.timed_wait(lock, posix_time::milliseconds(5000));
    
    ASSERT_EQ(2, answerHandled);
}


