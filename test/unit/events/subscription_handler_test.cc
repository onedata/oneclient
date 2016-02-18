/**
 * @file subscription_handler_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "events/subscriptionHandler.h"
#include "events/subscriptions/subscriptionCancellation.h"

#include "messages.pb.h"

#include <gmock/gmock.h>

#include <functional>
#include <memory>

using namespace ::testing;
using namespace one::client::events;

template <class EventType> class LowerLayer {
public:
    using EventT = EventType;
    using SubscriptionPtr = std::unique_ptr<typename EventT::Subscription>;
    using Buffer = EventBuffer<EventT>;
    using BufferPtr = std::unique_ptr<Buffer>;
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    void setEventBuffer(std::unique_ptr<EventBufferMap<EventT>>)
    {
        setEventBufferMap();
    }
    void setEventBuffer(std::unique_ptr<VoidEventBuffer<EventT>>)
    {
        setVoidEventBuffer();
    }

    MOCK_METHOD0_T(setEventBufferMap, void());
    MOCK_METHOD0_T(setVoidEventBuffer, void());
    MOCK_METHOD1_T(send, void(const SubscriptionCancellation &));
};

template <class EventT> class SubscriptionHandlerTest : public ::testing::Test {
protected:
    SubscriptionHandler<LowerLayer<EventT>> handler;
};

TYPED_TEST_CASE(SubscriptionHandlerTest, TestEventTypes);

TYPED_TEST(SubscriptionHandlerTest,
    subscribeShouldNotSetEventBufferIsSubscriptionIsEmpty)
{
    EXPECT_CALL(this->handler.mock, setEventBufferMap()).Times(0);
    EXPECT_CALL(this->handler.mock, setVoidEventBuffer()).Times(0);
    this->handler.subscribe(testSubscriptionPtr(1));
}

TYPED_TEST(SubscriptionHandlerTest,
    subscribeShouldSetEventBufferMapForFirstSubscription)
{
    EXPECT_CALL(this->handler.mock, setEventBufferMap()).Times(1);
    this->handler.subscribe(testSubscriptionPtr(1, 1));
}

TYPED_TEST(SubscriptionHandlerTest,
    unsubscribeHandlerForTheLastSubscriptionShouldSetVoidEventBuffer)
{
    EXPECT_CALL(this->handler.mock, setVoidEventBuffer()).Times(1);
    EXPECT_CALL(this->handler.mock, setEventBufferMap()).Times(1);
    auto unsubscribe = this->handler.subscribe(testSubscriptionPtr(1, 1));
    unsubscribe();
}
