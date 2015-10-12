/**
 * @file event_subscriptions_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "events/subscriptions/ioEventSubscription.h"
#include "events/subscriptions/eventSubscriptionCancellation.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"

#include "messages.pb.h"

#include <chrono>
#include <vector>
#include <algorithm>

using namespace one::client::events;
using namespace std::literals::chrono_literals;

template <class EventT> class IOEventSubscriptionTest : public ::testing::Test {
public:
    IOEventSubscriptionTest()
        : sub{1, 10, 10ms, 10}
    {
    }

protected:
    IOEventSubscription<EventT> sub;
};

TYPED_TEST_CASE(IOEventSubscriptionTest, AllEventTypes);

class EventSubscriptionCancellationTest : public ::testing::Test {
};

TYPED_TEST(IOEventSubscriptionTest, aggregationIdentityElement)
{
    IOEventSubscription<TypeParam> subIdent{};
    EXPECT_EQ(0, subIdent.id());
    EXPECT_TRUE(subIdent.counterThresholds().empty());
    EXPECT_TRUE(subIdent.timeThresholds().empty());
    EXPECT_TRUE(subIdent.sizeThresholds().empty());
}

TYPED_TEST(IOEventSubscriptionTest, aggregatesSubscriptions)
{
    std::vector<IOEventSubscription<TypeParam>> subs{
        {2, 20, 5ms, 20}, {3, 5, 20ms, 20}, {4, 20, 20ms, 5}};

    for (unsigned int i = 0; i < subs.size(); ++i) {
        this->sub += subs[i];
        EXPECT_EQ(1, this->sub.id());
        EXPECT_EQ(i + 2, this->sub.counterThresholds().size());
        EXPECT_EQ(i + 2, this->sub.timeThresholds().size());
        EXPECT_EQ(i + 2, this->sub.sizeThresholds().size());
    }

    EXPECT_EQ(5, *(this->sub.counterThresholds().begin()));
    EXPECT_EQ(5, this->sub.timeThresholds().begin()->count());
    EXPECT_EQ(5, *(this->sub.sizeThresholds().begin()));
}

TYPED_TEST(IOEventSubscriptionTest, subtractsSubscriptions)
{
    std::vector<IOEventSubscription<TypeParam>> subs{
        {2, 20, 5ms, 20}, {3, 5, 20ms, 20}, {4, 20, 20ms, 5}};

    for (unsigned int i = 0; i < subs.size(); ++i) {
        this->sub += subs[i];
    }

    for (unsigned int i = subs.size(); i > 0; --i) {
        this->sub -= subs[i - 1];
        EXPECT_EQ(1, this->sub.id());
        EXPECT_EQ(i, this->sub.counterThresholds().size());
        EXPECT_EQ(i, this->sub.timeThresholds().size());
        EXPECT_EQ(i, this->sub.sizeThresholds().size());
    }

    EXPECT_EQ(10, *(this->sub.counterThresholds().begin()));
    EXPECT_EQ(10, this->sub.timeThresholds().begin()->count());
    EXPECT_EQ(10, *(this->sub.sizeThresholds().begin()));
}

TYPED_TEST(IOEventSubscriptionTest, empty)
{
    IOEventSubscription<TypeParam> subIdent{};
    EXPECT_TRUE(subIdent.empty());
}

TYPED_TEST(IOEventSubscriptionTest, notEmpty)
{
    EXPECT_FALSE(this->sub.empty());
}

TYPED_TEST(IOEventSubscriptionTest, satisfied)
{
    EXPECT_TRUE(this->sub.satisfied(11, 10));
    EXPECT_TRUE(this->sub.satisfied(10, 9));
    EXPECT_TRUE(this->sub.satisfied(10, 10));
    EXPECT_TRUE(this->sub.satisfied(9, 10));
    EXPECT_TRUE(this->sub.satisfied(10, 11));
    EXPECT_TRUE(this->sub.satisfied(11, 11));
}

TYPED_TEST(IOEventSubscriptionTest, notSatisfied)
{
    IOEventSubscription<TypeParam> subIdent{};
    EXPECT_FALSE(subIdent.satisfied(0, 0));
    EXPECT_FALSE(subIdent.satisfied(10, 10));
    EXPECT_FALSE(this->sub.satisfied(9, 9));
}

TYPED_TEST(IOEventSubscriptionTest, deserializes)
{
    typename TypeParam::Subscription subMsg{};
    subMsg.set_id(1);
    subMsg.set_counter_threshold(10);
    subMsg.set_time_threshold(10);
    subMsg.set_size_threshold(10);
    IOEventSubscription<TypeParam> s{subMsg};
    EXPECT_EQ(1, s.id());
    EXPECT_EQ(10, *s.counterThresholds().begin());
    EXPECT_EQ(10, s.timeThresholds().begin()->count());
    EXPECT_EQ(10, *s.sizeThresholds().begin());
}

TEST_F(EventSubscriptionCancellationTest, deserializes)
{
    one::clproto::EventSubscriptionCancellation subCanMsg{};
    subCanMsg.set_id(1);
    EventSubscriptionCancellation subCan{subCanMsg};
    EXPECT_EQ(1, subCan.id());
}
