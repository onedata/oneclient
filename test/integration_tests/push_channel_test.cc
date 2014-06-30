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

#include <chrono>
#include <condition_variable>
#include <mutex>

using namespace boost::filesystem;
using namespace std;
using namespace std::placeholders;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;
using namespace veil::client::utils;

class PushChannelTest: public CommonIntegrationTest
{
protected:
    path directIO_root;

    std::mutex cbMutex;
    std::condition_variable cbCond;
    int answerHandled;
    
    PushChannelTest()
        : CommonIntegrationTest{std::unique_ptr<veil::testing::VeilFSMount>{new veil::testing::VeilFSMount{"main", "peer.pem"}}}
    {
    }
    
    void SetUp() override
    {
        CommonIntegrationTest::SetUp();
        answerHandled = 0;
    }
    
public:

    bool handler(const protocol::communication_protocol::Answer &msg, int waitFor)
    {
        std::unique_lock<std::mutex> lock(cbMutex);
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
    std::shared_ptr<CommunicationHandler> conn = context->getConnectionPool()->selectConnection();

    conn->closeConnection();
    int connectRes = conn->openConnection();
    ASSERT_EQ(0, connectRes);
    sleep(2);

    ASSERT_LT(0, fromString<int>(erlExec(string("{get_handler_count, \"") + config->getFuseID() + string("\"}"))));
}


// Test if PUSH channel receives messages
TEST_F(PushChannelTest, pushChannelInbox) {
    std::unique_lock<std::mutex> lock(cbMutex);

    // Register handler
    context->getPushListener()->subscribe(std::bind(&PushChannelTest::handler, this, _1, 2));

    // Send test message from cluster
    string sendAns = erlExec(string("{push_msg, \"test\", \"") + config->getFuseID() + "\"}");
    EXPECT_EQ("ok", sendAns);

    sendAns = erlExec(string("{push_msg, \"test\", \"") + config->getFuseID() + "\"}");
    EXPECT_EQ("ok", sendAns);

    cbCond.wait_for(lock, std::chrono::seconds(5));

    ASSERT_EQ(2, answerHandled);
}


