/**
 * @file scheduler_mock.h
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_SCHEDULER_MOCK_H
#define HELPERS_SCHEDULER_MOCK_H

#include <gmock/gmock.h>

#include <chrono>
#include <functional>

class MockScheduler {
public:
    MockScheduler()
    {
        using namespace ::testing;
        ON_CALL(*this, schedule(_, _)).WillByDefault(Return([] {}));
    }

    MOCK_METHOD2(schedule,
        std::function<void()>(
            const std::chrono::milliseconds, std::function<void()>));
};

#endif // HELPERS_SCHEDULER_MOCK_H
