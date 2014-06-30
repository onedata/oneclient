/**
 * @file pushListener_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "options_mock.h"
#include "jobScheduler_mock.h"
#include "testCommon.h"
#include "pushListener.h"

#include <atomic>
#include <chrono>
#include <mutex>
#include <condition_variable>

using namespace veil::protocol::communication_protocol;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class PushListenerTest
: public ::testing::Test {
protected:
    COMMON_DEFS();

    std::mutex cbMutex;
    std::condition_variable cbCond;
    std::unique_ptr<PushListener> listener;

    std::atomic<int> answerHandled;

    virtual void SetUp() {
        COMMON_SETUP();
        listener.reset(new PushListener());
        answerHandled = 0;
    }

    virtual void TearDown() {
        COMMON_CLEANUP();
    }

public:

    bool handler(const Answer &msg, bool ret = true)
    {
        if(msg.worker_answer() == "test")
            ++answerHandled;

        cbCond.notify_all();

        return ret;
    }

};


TEST_F(PushListenerTest, simpleRegisterAndHandle)
{
    std::unique_lock<std::mutex> lock(cbMutex);
    Answer ans; // Message to handle
    ans.set_answer_status("ok");
    ans.set_worker_answer("test");

    listener->subscribe(std::bind(&PushListenerTest::handler, this, std::placeholders::_1, true));

    listener->onMessage(ans);
    ASSERT_TRUE(cbCond.wait_for(lock, std::chrono::seconds(10),
                                [&]{ return answerHandled == 1; }));

    listener->onMessage(ans);
    ASSERT_TRUE(cbCond.wait_for(lock, std::chrono::seconds(10),
                                [&]{ return answerHandled == 2; }));

    listener->onMessage(ans);
    listener->onMessage(ans);
    ASSERT_TRUE(cbCond.wait_for(lock, std::chrono::seconds(10),
                                [&]{ return answerHandled == 4; }));
}


TEST_F(PushListenerTest, removeHandler)
{
    std::unique_lock<std::mutex> lock(cbMutex);
    Answer ans; // Message to handle
    ans.set_answer_status("ok");
    ans.set_worker_answer("test");

    int handlerId = listener->subscribe(std::bind(&PushListenerTest::handler, this, std::placeholders::_1, false)); // Should be removed after first call

    listener->onMessage(ans);
    cbCond.wait_for(lock, std::chrono::milliseconds(500));

    listener->onMessage(ans);
    listener->onMessage(ans);
    cbCond.wait_for(lock, std::chrono::milliseconds(500));


    ASSERT_EQ(1, answerHandled);

    answerHandled = 0;
    handlerId = listener->subscribe(std::bind(&PushListenerTest::handler, this, std::placeholders::_1, true));

    listener->onMessage(ans);
    cbCond.wait_for(lock, std::chrono::milliseconds(500));

    listener->unsubscribe(handlerId);

    listener->onMessage(ans);
    listener->onMessage(ans);
    cbCond.wait_for(lock, std::chrono::milliseconds(500));

    ASSERT_EQ(1, answerHandled);

}



