/**
 * @file push_channel_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication_protocol.pb.h"
#include "config.h"
#include "context.h"
#include "erlTestCore.h"
#include "fuse_messages.pb.h"
#include "pushListener.h"
#include "testCommon.h"

#include <boost/filesystem.hpp>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <chrono>
#include <condition_variable>
#include <mutex>

using namespace boost::filesystem;
using namespace std::placeholders;
using namespace one;
using namespace one::client::utils;
using namespace one::clproto::communication_protocol;
using namespace one::clproto::fuse_messages;

class PushChannelTest: public CommonIntegrationTest
{
protected:
    path directIO_root;

    std::mutex cbMutex;
    std::condition_variable cbCond;
    int answerHandled;

    PushChannelTest()
        : CommonIntegrationTest{std::unique_ptr<one::testing::FsImplMount>{new one::testing::FsImplMount{"main", "peer.pem"}}}
    {
    }

    void SetUp() override
    {
        CommonIntegrationTest::SetUp();
        answerHandled = 0;
    }

public:

    bool handler(const one::clproto::communication_protocol::Answer &msg, int waitFor)
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
TEST_F(PushChannelTest, RegisterAndClose)
{
    // By default client should register at least one handler
    ASSERT_LT(0, fromString<int>(one::testing::erlExec(std::string("{get_handler_count, \"") + config->getFuseID() + std::string("\"}"))));

    // Close communication channels
    context->setCommunicator(nullptr);

    // Close PUSH channel
    sleep(2);
    ASSERT_EQ(0, fromString<int>(one::testing::erlExec(std::string("{get_handler_count, \"") + config->getFuseID() + std::string("\"}"))));
}

// Test if PUSH channel receives messages
TEST_F(PushChannelTest, pushChannelInbox)
{
    std::unique_lock<std::mutex> lock(cbMutex);

    // Register handler
    context->getPushListener()->subscribe(std::bind(&PushChannelTest::handler, this, _1, 2));

    // Send test message from cluster
    std::string sendAns = one::testing::erlExec(std::string("{push_msg, \"test\", \"") + config->getFuseID() + "\"}");
    EXPECT_EQ("ok", sendAns);

    sendAns = one::testing::erlExec(std::string("{push_msg, \"test\", \"") + config->getFuseID() + "\"}");
    EXPECT_EQ("ok", sendAns);

    cbCond.wait_for(lock, std::chrono::seconds(5));

    ASSERT_EQ(2, answerHandled);
}


