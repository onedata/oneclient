/**
 * @file size_aggregator_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "events/aggregators/eventSizeAggregator.h"

#include <gmock/gmock.h>

#include <functional>
#include <memory>

using namespace ::testing;
using namespace one::client::events;
using namespace std::chrono_literals;

template <class EventT> class LowerLayer {
public:
    using EventPtr = std::unique_ptr<EventT>;
    using SubscriptionPtr = std::unique_ptr<TestSubscription>;
    using UnsubscribeHandler = std::function<void()>;
    using OnTriggerCallback = std::function<void()>;
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    void process(EventPtr event) { process(*event); }

    void trigger(EventPtr event) { trigger(*event); }

    UnsubscribeHandler subscribe(SubscriptionPtr subscription)
    {
        return subscribe(*subscription);
    };

    MOCK_METHOD0_T(trigger, void());
    MOCK_METHOD1_T(trigger, void(const EventT &));
    MOCK_METHOD1_T(process, void(const EventT &));
    MOCK_METHOD1_T(subscribe, UnsubscribeHandler(const TestSubscription &));
    MOCK_METHOD1_T(setOnTriggerCallback, void(OnTriggerCallback));
};

template <class EventT> class EventSizeAggregatorTest : public ::testing::Test {
public:
    EventSizeAggregatorTest()
    {
        ON_CALL(aggregator.mock, subscribe(_)).WillByDefault(Return([] {}));
        EXPECT_CALL(aggregator.mock, subscribe(_)).Times(AnyNumber());
    }

protected:
    EventSizeAggregator<LowerLayer<EventT>> aggregator;
};

TYPED_TEST_CASE(EventSizeAggregatorTest, TestEventTypes);

TYPED_TEST(EventSizeAggregatorTest, initializeShouldSetOnTriggerCallback)
{
    EXPECT_CALL(this->aggregator.mock, setOnTriggerCallback(_)).Times(1);
    this->aggregator.initializeAggregation();
}

TYPED_TEST(EventSizeAggregatorTest, processShouldForwardEventIfNoSubscripton)
{
    EXPECT_CALL(this->aggregator.mock, process(_)).Times(1);
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(EventSizeAggregatorTest,
    processShouldForwardEventIfSizeThresholdNotExceeded)
{
    EXPECT_CALL(this->aggregator.mock, process(_)).Times(1);
    this->aggregator.subscribe(testSubscriptionPtr(1, 0, 0ms, 20));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(
    EventSizeAggregatorTest, processShouldTriggerEmissionIfThresholdExceeded)
{
    EXPECT_CALL(this->aggregator.mock, trigger(_)).Times(1);
    this->aggregator.subscribe(testSubscriptionPtr(1, 0, 0ms, 10));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(
    EventSizeAggregatorTest, setOnTriggerCallbackShouldWrapOnTriggerCallback)
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

TYPED_TEST(EventSizeAggregatorTest, onTriggerCallbackShouldResetAggregatorSize)
{
    typename LowerLayer<TypeParam>::OnTriggerCallback callback;
    EXPECT_CALL(this->aggregator.mock, setOnTriggerCallback(_))
        .Times(1)
        .WillOnce(SaveArg<0>(&callback));
    EXPECT_CALL(this->aggregator.mock, process(_)).Times(2);
    EXPECT_CALL(this->aggregator.mock, trigger(_)).Times(0);

    this->aggregator.initializeAggregation();
    this->aggregator.subscribe(testSubscriptionPtr(1, 0, 0ms, 20));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
    callback();
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(EventSizeAggregatorTest,
    subscribeShouldForwardSubscriptionIfSizeThresholdNotSet)
{
    EXPECT_CALL(this->aggregator.mock, subscribe(_)).Times(1);
    this->aggregator.subscribe(testSubscriptionPtr(1));
}

TYPED_TEST(
    EventSizeAggregatorTest, subscribeShouldReturnWrappedUnsubscribeHandler)
{
    bool called = false;
    EXPECT_CALL(this->aggregator.mock, subscribe(_))
        .Times(1)
        .WillOnce(Return([&] { called = true; }));
    auto handler =
        this->aggregator.subscribe(testSubscriptionPtr(1, 0, 0ms, 10));
    handler();
    EXPECT_TRUE(called);
}

TYPED_TEST(EventSizeAggregatorTest,
    subscriptionRemovalHandlerShouldReduceSizeThresholds)
{
    EXPECT_CALL(this->aggregator.mock, subscribe(_)).Times(1);
    EXPECT_CALL(this->aggregator.mock, process(_)).Times(1);
    EXPECT_CALL(this->aggregator.mock, trigger(_)).Times(1);
    auto handler =
        this->aggregator.subscribe(testSubscriptionPtr(1, 0, 0ms, 10));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
    handler();
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(EventSizeAggregatorTest,
    removalOfTheLastSizeThresholdShouldResetAggregatorSize)
{
    EXPECT_CALL(this->aggregator.mock, subscribe(_)).Times(2);
    EXPECT_CALL(this->aggregator.mock, process(_)).Times(2);
    EXPECT_CALL(this->aggregator.mock, trigger(_)).Times(0);
    auto handler =
        this->aggregator.subscribe(testSubscriptionPtr(1, 0, 0ms, 20));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
    handler();
    this->aggregator.subscribe(testSubscriptionPtr(2, 2));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(EventSizeAggregatorTest, subscribeShouldMinimizeSizeThresholds)
{
    EXPECT_CALL(this->aggregator.mock, subscribe(_)).Times(2);
    EXPECT_CALL(this->aggregator.mock, process(_)).Times(1);
    EXPECT_CALL(this->aggregator.mock, trigger(_)).Times(1);
    this->aggregator.subscribe(testSubscriptionPtr(1, 0, 0ms, 30));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
    this->aggregator.subscribe(testSubscriptionPtr(2, 0, 0ms, 20));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
}

TYPED_TEST(EventSizeAggregatorTest,
    subscriptionWithSizeThresholdLessThanAggregatorSizeShouldTriggerEmission)
{
    EXPECT_CALL(this->aggregator.mock, subscribe(_)).Times(2);
    EXPECT_CALL(this->aggregator.mock, process(_)).Times(1);
    EXPECT_CALL(this->aggregator.mock, trigger()).Times(1);
    this->aggregator.subscribe(testSubscriptionPtr(1, 0, 0ms, 20));
    this->aggregator.process(std::make_unique<TypeParam>("fileUuid"));
    this->aggregator.subscribe(testSubscriptionPtr(2, 0, 0ms, 10));
}
