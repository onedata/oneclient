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
#include <boost/thread.hpp>
#include <boost/thread/thread_time.hpp>

using namespace ::testing;
using namespace veil::client;
using namespace veil::protocol::communication_protocol;

class PushListenerTest: public CommonTest
{
protected:
    boost::mutex cbMutex;
    boost::condition cbCond;
    std::unique_ptr<PushListener> listener;
    
    int answerHandled;
    
    void SetUp() override
    {
        CommonTest::SetUp();
        listener.reset(new PushListener(context));
        answerHandled = 0;
    }
    
public:
    
    bool handler(const Answer &msg, bool ret = true)
    {
        boost::unique_lock<boost::mutex> lock(cbMutex);
        if(msg.worker_answer() == "test")
            answerHandled++;
        cbCond.notify_all();
        
        return ret;
    }
};


TEST_F(PushListenerTest, simpleRegisterAndHandle)
{
    boost::unique_lock<boost::mutex> lock(cbMutex);
    Answer ans; // Message to handle
    ans.set_answer_status("ok");
    ans.set_worker_answer("test");
    
    listener->subscribe(boost::bind(&PushListenerTest::handler, this, _1, true));
    
    listener->onMessage(ans);
    cbCond.timed_wait(lock, boost::posix_time::milliseconds(500));
    
    listener->onMessage(ans);
    cbCond.timed_wait(lock, boost::posix_time::milliseconds(500));
    
    listener->onMessage(ans);
    listener->onMessage(ans);
    cbCond.timed_wait(lock, boost::posix_time::milliseconds(500));
    
    
    ASSERT_EQ(4, answerHandled);
    
}


TEST_F(PushListenerTest, removeHandler)
{
    boost::unique_lock<boost::mutex> lock(cbMutex);
    Answer ans; // Message to handle
    ans.set_answer_status("ok");
    ans.set_worker_answer("test");
    
    int handlerId = listener->subscribe(boost::bind(&PushListenerTest::handler, this, _1, false)); // Should be removed after first call
    
    listener->onMessage(ans);
    cbCond.timed_wait(lock, boost::posix_time::milliseconds(500));
    
    listener->onMessage(ans);
    listener->onMessage(ans);
    cbCond.timed_wait(lock, boost::posix_time::milliseconds(500));
    
    
    ASSERT_EQ(1, answerHandled);
    
    answerHandled = 0;
    handlerId = listener->subscribe(boost::bind(&PushListenerTest::handler, this, _1, true));
    
    listener->onMessage(ans);
    cbCond.timed_wait(lock, boost::posix_time::milliseconds(500));
    
    listener->unsubscribe(handlerId);
    
    listener->onMessage(ans);
    listener->onMessage(ans);
    cbCond.timed_wait(lock, boost::posix_time::milliseconds(500));
    
    ASSERT_EQ(1, answerHandled);
    
}



