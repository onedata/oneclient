/**
 * @file subscription_registry_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "events/subscriptionRegistry.h"
#include "events/subscriptions/eventSubscriptionCancellation.h"
#include "scheduler_mock.h"

#include <gtest/gtest.h>

#include <memory>

using namespace ::testing;
using namespace one::client;
using namespace one::client::events;

class SubscriptionRegistryTest : public ::testing::Test {
public:
    SubscriptionRegistryTest()
    {
        ctx = std::make_shared<Context>();
        sched = std::make_shared<NiceMock<MockScheduler>>();
        ctx->setScheduler(sched);
        subReg = std::make_unique<SubscriptionRegistry>(ctx);

        ON_CALL(*sched, post(_, _))
            .WillByDefault(WithArgs<1>(
                Invoke([](const std::function<void()> &task) { task(); })));
    }

protected:
    std::shared_ptr<Context> ctx;
    std::shared_ptr<NiceMock<MockScheduler>> sched;
    std::unique_ptr<SubscriptionRegistry> subReg;
};

TEST_F(SubscriptionRegistryTest, registryShouldAddSubscriptionsWithDifferentId)
{
    std::pair<uint64_t, std::function<void()>> sub1{1, [] {}};
    EXPECT_TRUE(subReg->add(std::move(sub1)).get());
    std::pair<uint64_t, std::function<void()>> sub2{2, [] {}};
    EXPECT_TRUE(subReg->add(std::move(sub2)).get());
}

TEST_F(SubscriptionRegistryTest, registryShouldNotAddSubscriptionsWithTheSameId)
{
    bool cancelled = false;
    std::pair<uint64_t, std::function<void()>> sub1{1, [] {}};
    std::pair<uint64_t, std::function<void()>> sub2{
        1, [&] { cancelled = true; }};
    subReg->add(std::move(sub1));
    EXPECT_FALSE(subReg->add(std::move(sub2)).get());
    EXPECT_TRUE(cancelled);
}

TEST_F(
    SubscriptionRegistryTest, registryShouldAddTheSameSubscriptionAfterRemoval)
{
    std::pair<uint64_t, std::function<void()>> sub1{1, [] {}};
    std::pair<uint64_t, std::function<void()>> sub2{1, [] {}};
    subReg->add(std::move(sub1));
    subReg->remove(EventSubscriptionCancellation{1});
    EXPECT_TRUE(subReg->add(std::move(sub2)).get());
}

TEST_F(SubscriptionRegistryTest, registryShouldNotCancelNotExistingSubscription)
{
    EXPECT_FALSE(subReg->remove(EventSubscriptionCancellation{1}).get());
}

TEST_F(SubscriptionRegistryTest, registryShouldCancelExistingSubscription)
{
    bool subCancelled = false;
    std::pair<uint64_t, std::function<void()>> sub{
        1, [&] { subCancelled = true; }};
    subReg->add(std::move(sub));
    subReg->remove(EventSubscriptionCancellation{1});
    EXPECT_TRUE(subCancelled);
}
