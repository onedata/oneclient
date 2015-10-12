/**
 * @file scheduler_mock.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_SCHEDULER_MOCK_H
#define HELPERS_SCHEDULER_MOCK_H

#include "scheduler.h"

#include <gmock/gmock.h>

class MockScheduler : public one::Scheduler {
public:
    MockScheduler()
        : one::Scheduler{0}
    {
    }

    MOCK_METHOD2(
        post, void(const asio::io_service::strand &, std::function<void()>));
};

#endif // HELPERS_SCHEDULER_MOCK_H
