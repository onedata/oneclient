/**
 * @file time_aggregator_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "events/aggregators/eventTimeAggregator.h"
#include "scheduler_mock.h"

#include <gmock/gmock.h>

#include <functional>
#include <memory>

using namespace ::testing;
using namespace one;
using namespace one::client::events;
using namespace std::chrono_literals;

template <class EventT> class LowerLayer {
public:
    using SubscriptionPtr = std::unique_ptr<TestSubscription>;
    using UnsubscribeHandler = std::function<void()>;
    using OnTriggerCallback = std::function<void()>;
    using PeriodicTriggerHandler = std::function<void()>;
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    UnsubscribeHandler subscribe(SubscriptionPtr subscription)
    {
        return subscribe(*subscription);
    };

    MOCK_METHOD0_T(trigger, void());
    MOCK_METHOD1_T(subscribe, UnsubscribeHandler(const TestSubscription &));
    MOCK_METHOD1_T(setOnTriggerCallback, void(OnTriggerCallback));
};

template <class EventT> class EventTimeAggregatorTest : public ::testing::Test {
public:
    EventTimeAggregatorTest()
        : scheduler{std::make_shared<NiceMock<MockScheduler>>()}
    {
        aggregator.setScheduler(scheduler);
        aggregator.setPeriodicTriggerHandler([] {});

        ON_CALL(aggregator.mock, subscribe(_)).WillByDefault(Return([] {}));
        ON_CALL(*scheduler, schedule(_, _))
            .WillByDefault(
                WithArgs<1>(Invoke([](const std::function<void()> &task) {
                    task();
                    return [] {};
                })));
        EXPECT_CALL(aggregator.mock, subscribe(_)).Times(AnyNumber());
    }

protected:
    std::shared_ptr<NiceMock<MockScheduler>> scheduler;
    EventTimeAggregator<LowerLayer<EventT>, NiceMock<MockScheduler>> aggregator;
};

TYPED_TEST_CASE(EventTimeAggregatorTest, TestEventTypes);

TYPED_TEST(EventTimeAggregatorTest, initializeShouldSetOnTriggerCallback)
{
    EXPECT_CALL(this->aggregator.mock, setOnTriggerCallback(_)).Times(1);
    this->aggregator.initializeAggregation();
}

TYPED_TEST(
    EventTimeAggregatorTest, setOnTriggerCallbackShouldWrapOnTriggerCallback)
{
    typename LowerLayer<TypeParam>::OnTriggerCallback callback;
    EXPECT_CALL(this->aggregator.mock, setOnTriggerCallback(_))
        .Times(1)
        .WillOnce(SaveArg<0>(&callback));

    bool called = false;
    this->aggregator.setOnTriggerCallback([&] { called = true; });
    callback();
    EXPECT_TRUE(called);
}

TYPED_TEST(
    EventTimeAggregatorTest, onTriggerCallbackShouldResetPeriodicEmission)
{
    typename LowerLayer<TypeParam>::OnTriggerCallback callback;
    EXPECT_CALL(this->aggregator.mock, setOnTriggerCallback(_))
        .Times(1)
        .WillOnce(SaveArg<0>(&callback));
    bool called = false;
    this->aggregator.setPeriodicTriggerHandler([&] { called = true; });
    this->aggregator.subscribe(testSubscriptionPtr(1, 0, 100ms));

    this->aggregator.initializeAggregation();
    callback();
    EXPECT_TRUE(called);
}

TYPED_TEST(EventTimeAggregatorTest,
    subscribeShouldForwardSubscriptionIfTimeThresholdNotSet)
{
    EXPECT_CALL(this->aggregator.mock, subscribe(_)).Times(1);
    this->aggregator.subscribe(testSubscriptionPtr(1));
}

TYPED_TEST(
    EventTimeAggregatorTest, subscribeShouldReturnWrappedUnsubscribeHandler)
{
    bool called = false;
    EXPECT_CALL(this->aggregator.mock, subscribe(_))
        .Times(1)
        .WillOnce(Return([&] { called = true; }));
    auto handler = this->aggregator.subscribe(testSubscriptionPtr(1, 0, 100ms));
    handler();
    EXPECT_TRUE(called);
}

TYPED_TEST(EventTimeAggregatorTest,
    removalOfTheLastTimeThresholdShouldResetPeriodicEmission)
{
    EXPECT_CALL(this->aggregator.mock, subscribe(_)).Times(1);
    bool called = false;
    EXPECT_CALL(*this->scheduler, schedule(_, _))
        .Times(1)
        .WillOnce(Return([&] { called = true; }));
    auto handler = this->aggregator.subscribe(testSubscriptionPtr(1, 0, 100ms));
    handler();
    EXPECT_TRUE(called);
}

TYPED_TEST(EventTimeAggregatorTest,
    subscriptionWithTimeThresholdLessThanPeriodicEmissionShouldTriggerEmission)
{
    EXPECT_CALL(this->aggregator.mock, subscribe(_)).Times(2);
    int counter = 0;
    this->aggregator.setPeriodicTriggerHandler([&] { ++counter; });
    this->aggregator.subscribe(testSubscriptionPtr(1, 0, 200ms));
    this->aggregator.subscribe(testSubscriptionPtr(2, 0, 100ms));
    EXPECT_EQ(2, counter);
}
