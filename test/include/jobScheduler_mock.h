/**
 * @file jobScheduler_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef JOB_SCHEDULER_MOCK_H
#define JOB_SCHEDULER_MOCK_H


#include "jobScheduler.h"

#include <gmock/gmock.h>

class MockJobScheduler: public veil::client::JobScheduler
{
public:
    MockJobScheduler()
    {
        using namespace ::testing;
        EXPECT_CALL(*this, deleteJobs(_, _)).Times(AtLeast(0));
    }

    MOCK_METHOD1(addTask, void(veil::client::Job));
    MOCK_METHOD2(deleteJobs, void(const veil::client::ISchedulable * const,
                                  const veil::client::ISchedulable::TaskID));
};


#endif // JOB_SCHEDULER_MOCK_H
