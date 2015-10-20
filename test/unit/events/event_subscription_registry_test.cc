/**
 * @file event_subscription_registry_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "events/subscriptionRegistry.h"

#include <functional>

using namespace one::client::events;

class SubscriptionRegistryTest : public ::testing::Test {
protected:
    SubscriptionRegistry registry;
};

TEST_F(SubscriptionRegistryTest, addUnsubscribeHandlerShouldAddHandler)
{
    EXPECT_TRUE(registry.addUnsubscribeHandler(1, [] {}));
}

TEST_F(SubscriptionRegistryTest,
    addUnsubscribeHandlerShouldNotAddHandlerWithTheSameIdTwice)
{
    registry.addUnsubscribeHandler(1, [] {});
    EXPECT_FALSE(registry.addUnsubscribeHandler(1, [] {}));
}

TEST_F(SubscriptionRegistryTest,
    removeSubscriptionShouldRemoveExistingSubscription)
{
    registry.addUnsubscribeHandler(1, [] {});
    EXPECT_TRUE(registry.removeSubscription(1));
}

TEST_F(SubscriptionRegistryTest,
    removeSubscriptionShouldNotRemoveNonExistingSubscription)
{
    EXPECT_FALSE(registry.removeSubscription(1));
}

TEST_F(SubscriptionRegistryTest, removeSubscriptionShouldCallUnsubscribeHandler)
{
    bool called = false;
    registry.addUnsubscribeHandler(1, [&] { called = true; });
    registry.removeSubscription(1);
    EXPECT_TRUE(called);
}
