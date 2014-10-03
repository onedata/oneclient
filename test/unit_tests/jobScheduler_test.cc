/**
 * @file jobScheduler_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "jobScheduler_proxy.h"
#include "options_mock.h"
#include "jobScheduler_mock.h"

#include <memory>

using namespace ::testing;
using namespace one::client;

bool operator==(const Job &lhs, const Job &rhs)
{
    return lhs.subject == rhs.subject && lhs.task == rhs.task &&
           lhs.arg0 == rhs.arg0 && lhs.arg1 == rhs.arg1 &&
           lhs.arg2 == rhs.arg2;
}

class MockJobObject: public ISchedulable
{
public:
    MOCK_METHOD4(runTask, bool(TaskID, const std::string&, const std::string&, const std::string&));
};

class JobSchedulerTest: public CommonTest
{

protected:
    ProxyJobScheduler proxy;
    std::shared_ptr<MockJobObject> jobContext1_;
    std::shared_ptr<MockJobObject> jobContext2_;

    void SetUp() override
    {
        CommonTest::SetUp();
        jobContext1_ = std::make_shared<MockJobObject>();
        jobContext2_ = std::make_shared<MockJobObject>();
    }
};

TEST_F(JobSchedulerTest, JobCompare) {
    Job j1 = Job(1, jobContext1_, jobContext1_->TASK_CLEAR_FILE_ATTR, "a", "b", "c");
    Job j2 = Job(10, jobContext1_, jobContext1_->TASK_CLEAR_FILE_ATTR, "a", "b", "c");
    Job j3 = Job(100, jobContext2_, jobContext1_->TASK_CLEAR_FILE_ATTR, "d", "b", "");
    Job j4 = Job(100, jobContext2_, jobContext1_->TASK_SEND_FILE_NOT_USED, "d", "b", "");
    Job j5 = Job(10, jobContext2_, jobContext1_->TASK_SEND_FILE_NOT_USED, "d", "b", "");
    Job j6 = Job(1, jobContext2_, jobContext1_->TASK_CLEAR_FILE_ATTR, "a", "b", "c");

    EXPECT_TRUE(j5 == j4);
    EXPECT_FALSE(j1== j3);
    EXPECT_FALSE(j3 == j4);
    EXPECT_FALSE(j1 == j4);
    EXPECT_FALSE(j2 == j4);
    EXPECT_FALSE(j1 == j6);

    EXPECT_LT(j1, j2);
    EXPECT_LT(j1, j3);
    EXPECT_LT(j5, j3);
}

TEST_F(JobSchedulerTest, AddAndDelete) {
    EXPECT_CALL(*jobContext1_, runTask(jobContext1_->TASK_CLEAR_FILE_ATTR, "a", "b", "c")).WillOnce(Return(true));
    proxy.addTask(Job(time(NULL) - 1, jobContext1_, jobContext1_->TASK_CLEAR_FILE_ATTR, "a", "b", "c"));
    EXPECT_CALL(*jobContext1_, runTask(jobContext1_->TASK_SEND_FILE_NOT_USED, "a", "b", "")).WillOnce(Return(true));
    proxy.addTask(Job(time(NULL) - 1, jobContext1_, jobContext1_->TASK_SEND_FILE_NOT_USED, "a", "b", ""));

    sleep(1);

    {
        InSequence s;
        EXPECT_CALL(*jobContext1_, runTask(jobContext1_->TASK_SEND_FILE_NOT_USED, "a1", "b", "1"));
        EXPECT_CALL(*jobContext2_, runTask(jobContext1_->TASK_SEND_FILE_NOT_USED, "a2", "b", "2"));
        EXPECT_CALL(*jobContext2_, runTask(jobContext1_->TASK_SEND_FILE_NOT_USED, "a3", "b", "3"));
        EXPECT_CALL(*jobContext1_, runTask(jobContext1_->TASK_SEND_FILE_NOT_USED, "a4", "b", "4"));
    }

    time_t currT = time(NULL);
    proxy.addTask(Job(currT - 4, jobContext1_, jobContext1_->TASK_SEND_FILE_NOT_USED, "a1", "b", "1"));
    proxy.addTask(Job(currT - 3, jobContext2_, jobContext1_->TASK_SEND_FILE_NOT_USED, "a2", "b", "2"));
    proxy.addTask(Job(currT - 2, jobContext2_, jobContext1_->TASK_SEND_FILE_NOT_USED, "a3", "b", "3"));
    proxy.addTask(Job(currT - 1, jobContext1_, jobContext1_->TASK_SEND_FILE_NOT_USED, "a4", "b", "4"));

    proxy.addTask(Job(currT + 4, jobContext1_, jobContext1_->TASK_SEND_FILE_NOT_USED, "2", "2", "2"));
    proxy.addTask(Job(currT + 3, jobContext1_, jobContext1_->TASK_SEND_FILE_NOT_USED, "1", "1", "1"));
    proxy.addTask(Job(currT + 4, jobContext1_, jobContext1_->TASK_CLEAR_FILE_ATTR, "1", "2", "3"));
    proxy.addTask(Job(currT + 4, jobContext2_, jobContext1_->TASK_SEND_FILE_NOT_USED, "1", "2", "3"));
    proxy.addTask(Job(currT + 4, jobContext2_, jobContext1_->TASK_SEND_FILE_NOT_USED, "4", "5", "6"));

    sleep(1);

    proxy.deleteJobs(jobContext2_.get(), jobContext1_->TASK_LAST_ID);
    proxy.deleteJobs(jobContext1_.get(), jobContext1_->TASK_CLEAR_FILE_ATTR);

    sleep(1);

    {
        InSequence s;
        EXPECT_CALL(*jobContext1_, runTask(jobContext1_->TASK_SEND_FILE_NOT_USED, "1", "1", "1")).Times(1);
        EXPECT_CALL(*jobContext1_, runTask(jobContext1_->TASK_SEND_FILE_NOT_USED, "2", "2", "2")).Times(1);
    }

    sleep(4);
}
