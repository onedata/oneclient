/**
 * @file subscriptions_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "events/subscriptions/readSubscription.h"
#include "events/subscriptions/subscriptionCancellation.h"
#include "events/subscriptions/writeSubscription.h"

#include "messages.pb.h"

#include <chrono>

using namespace one::client::events;
using namespace std::literals::chrono_literals;

class ReadSubscriptionTest : public ::testing::Test {
public:
    ReadSubscriptionTest()
        : subscription{1, 10, 10ms, 10}
    {
    }

protected:
    ReadSubscription subscription;
};

class WriteSubscriptionTest : public ::testing::Test {
public:
    WriteSubscriptionTest()
        : subscription{1, 10, 10ms, 10}
    {
    }

protected:
    WriteSubscription subscription;
};

class SubscriptionCancellationTest : public ::testing::Test {
};

TEST_F(ReadSubscriptionTest, deserializationShouldProduceEmptySubscription)
{
    one::clproto::ReadSubscription message{};

    ReadSubscription s{1, message};

    EXPECT_EQ(1, s.id());
    EXPECT_FALSE(s.counterThreshold());
    EXPECT_FALSE(s.timeThreshold());
    EXPECT_FALSE(s.sizeThreshold());
    EXPECT_TRUE(s.empty());
}

TEST_F(ReadSubscriptionTest, deserializationShouldSetCounterThreshold)
{
    one::clproto::ReadSubscription message{};
    message.set_counter_threshold(10);

    ReadSubscription s{1, message};

    EXPECT_EQ(1, s.id());
    EXPECT_TRUE(s.counterThreshold());
    EXPECT_EQ(10, s.counterThreshold().get());
    EXPECT_FALSE(s.timeThreshold());
    EXPECT_FALSE(s.sizeThreshold());
    EXPECT_FALSE(s.empty());
}

TEST_F(ReadSubscriptionTest, deserializationShouldSetTimeThreshold)
{
    one::clproto::ReadSubscription message{};
    message.set_time_threshold(10);

    ReadSubscription s{1, message};

    EXPECT_EQ(1, s.id());
    EXPECT_FALSE(s.counterThreshold());
    EXPECT_TRUE(s.timeThreshold());
    EXPECT_EQ(10, s.timeThreshold().get().count());
    EXPECT_FALSE(s.sizeThreshold());
    EXPECT_FALSE(s.empty());
}

TEST_F(ReadSubscriptionTest, deserializationShouldSetSizeThreshold)
{
    one::clproto::ReadSubscription message{};
    message.set_size_threshold(10);

    ReadSubscription s{1, message};

    EXPECT_EQ(1, s.id());
    EXPECT_FALSE(s.counterThreshold());
    EXPECT_FALSE(s.timeThreshold());
    EXPECT_TRUE(s.sizeThreshold());
    EXPECT_EQ(10, s.sizeThreshold().get());
    EXPECT_FALSE(s.empty());
}

TEST_F(WriteSubscriptionTest, deserializationShouldProduceEmptySubscription)
{
    one::clproto::WriteSubscription message{};

    WriteSubscription s{1, message};

    EXPECT_EQ(1, s.id());
    EXPECT_FALSE(s.counterThreshold());
    EXPECT_FALSE(s.timeThreshold());
    EXPECT_FALSE(s.sizeThreshold());
    EXPECT_TRUE(s.empty());
}

TEST_F(WriteSubscriptionTest, deserializationShouldSetCounterThreshold)
{
    one::clproto::WriteSubscription message{};
    message.set_counter_threshold(10);

    WriteSubscription s{1, message};

    EXPECT_EQ(1, s.id());
    EXPECT_TRUE(s.counterThreshold());
    EXPECT_EQ(10, s.counterThreshold().get());
    EXPECT_FALSE(s.timeThreshold());
    EXPECT_FALSE(s.sizeThreshold());
    EXPECT_FALSE(s.empty());
}

TEST_F(WriteSubscriptionTest, deserializationShouldSetTimeThreshold)
{
    one::clproto::WriteSubscription message{};
    message.set_time_threshold(10);

    WriteSubscription s{1, message};

    EXPECT_EQ(1, s.id());
    EXPECT_FALSE(s.counterThreshold());
    EXPECT_TRUE(s.timeThreshold());
    EXPECT_EQ(10, s.timeThreshold().get().count());
    EXPECT_FALSE(s.sizeThreshold());
    EXPECT_FALSE(s.empty());
}

TEST_F(WriteSubscriptionTest, deserializationShouldSetSizeThreshold)
{
    one::clproto::WriteSubscription message{};
    message.set_size_threshold(10);

    WriteSubscription s{1, message};

    EXPECT_EQ(1, s.id());
    EXPECT_FALSE(s.counterThreshold());
    EXPECT_FALSE(s.timeThreshold());
    EXPECT_TRUE(s.sizeThreshold());
    EXPECT_EQ(10, s.sizeThreshold().get());
    EXPECT_FALSE(s.empty());
}

TEST_F(SubscriptionCancellationTest, deserializationShouldSetId)
{
    one::clproto::SubscriptionCancellation message{};
    message.set_id(1);
    SubscriptionCancellation subscription{message};
    EXPECT_EQ(1, subscription.id());
}
