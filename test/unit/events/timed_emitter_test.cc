/**
 * @file timed_emitter_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "mocks/emitter_mock.h"
#include "mocks/manager_mock.h"
#include "scheduler_mock.h"
#include "utils.h"

#include <chrono>

using namespace ::testing;
using namespace one::client::events;
using namespace std::literals::chrono_literals;

template <class T> struct TimedEmitterTest : public ::testing::Test {
    MockScheduler mockScheduler;
    MockManager mockManager;
    MockEmitter<T> *mockEmitter = new MockEmitter<T>();
    TimedEmitter<T, MockScheduler> emitter{StreamKey::FILE_WRITTEN, 500ms,
        mockManager, mockScheduler,
        std::unique_ptr<MockEmitter<T>>(mockEmitter)};
};

TYPED_TEST_CASE(TimedEmitterTest, TestEventTypes);

TYPED_TEST(TimedEmitterTest, readyShouldReturnFalse)
{
    ASSERT_FALSE(this->emitter.ready());
}

TYPED_TEST(TimedEmitterTest, readyShouldForwardCall)
{
    this->emitter.ready();
    ASSERT_TRUE(this->mockEmitter->readyCalled);
}

TYPED_TEST(TimedEmitterTest, processShouldScheduleTask)
{
    std::chrono::milliseconds threshold;
    EXPECT_CALL(this->mockScheduler, schedule(_, _))
        .WillOnce(WithArgs<0>(Invoke([&](const auto &delay) {
            threshold = delay;
            return [] {};
        })));
    this->emitter.process(std::make_unique<TypeParam>("1"));
    ASSERT_EQ(500ms, threshold);
}

TYPED_TEST(TimedEmitterTest, processShouldScheduleTaskOnce)
{
    EXPECT_CALL(this->mockScheduler, schedule(_, _)).Times(1);
    this->emitter.process(std::make_unique<TypeParam>("1"));
    this->emitter.process(std::make_unique<TypeParam>("1"));
}

TYPED_TEST(TimedEmitterTest, scheduledTaskShouldFlushTheStream)
{
    EXPECT_CALL(this->mockScheduler, schedule(_, _))
        .WillOnce(WithArgs<1>(Invoke([](const auto &task) {
            task();
            return [] {};
        })));

    StreamKey streamKey;
    EXPECT_CALL(this->mockManager, flush(_)).WillOnce(SaveArg<0>(&streamKey));
    this->emitter.process(std::make_unique<TypeParam>("1"));
    ASSERT_EQ(StreamKey::FILE_WRITTEN, streamKey);
}

TYPED_TEST(TimedEmitterTest, processShouldForwardCall)
{
    this->emitter.process(std::make_unique<TypeParam>("1"));
    ASSERT_TRUE(this->mockEmitter->processCalled);
}

TYPED_TEST(TimedEmitterTest, resetShouldCancelPeriodicFlush)
{
    bool flushCancelled = false;

    EXPECT_CALL(this->mockScheduler, schedule(_, _)).WillOnce(Return([&] {
        flushCancelled = true;
    }));

    this->emitter.process(std::make_unique<TypeParam>("1"));
    this->emitter.reset();
    ASSERT_TRUE(flushCancelled);
}

TYPED_TEST(TimedEmitterTest, resetShouldForwardCall)
{
    this->emitter.reset();
    ASSERT_TRUE(this->mockEmitter->resetCalled);
}
