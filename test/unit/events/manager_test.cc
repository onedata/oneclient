/**
 * @file manager_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "utils.h"

using namespace one::client::events;

struct ManagerTest : public ::testing::Test {
    bool handlerCalled = false;
    TestSubscription subscription{[&](auto) { handlerCalled = true; }};
    Manager manager{testContext()};
};

TEST_F(ManagerTest, subscribeShouldReturnSubscriptionId)
{
    ASSERT_LT(this->manager.subscribe(this->subscription), 0);
}

TEST_F(ManagerTest, subscribeShouldCreateSubscription)
{
    auto id = this->manager.subscribe(this->subscription);
    ASSERT_TRUE(this->manager.existsSubscription(id));
}

TEST_F(ManagerTest, unsubscribeShouldIgnoreMissingSubscription)
{
    ASSERT_FALSE(this->manager.unsubscribe(0));
}

TEST_F(ManagerTest, subscriptionShouldNotExist)
{
    ASSERT_FALSE(this->manager.existsSubscription(0));
}

TEST_F(ManagerTest, unsubscribeShouldRemoveSubscription)
{
    auto id = this->manager.subscribe(this->subscription);
    ASSERT_TRUE(this->manager.unsubscribe(id));
}

TEST_F(ManagerTest, emitShouldNotCallHandler)
{
    this->manager.subscribe(this->subscription);
    this->manager.emit(std::make_unique<TestEvent>());
    ASSERT_FALSE(this->handlerCalled);
}

TEST_F(ManagerTest, emitShouldCallHandler)
{
    this->manager.subscribe(this->subscription);
    this->manager.emit(std::make_unique<TestEvent>());
    this->manager.emit(std::make_unique<TestEvent>());
    ASSERT_TRUE(this->handlerCalled);
}

TEST_F(ManagerTest, flushShouldCallHandler)
{
    this->manager.subscribe(this->subscription);
    this->manager.emit(std::make_unique<TestEvent>());
    ASSERT_FALSE(this->handlerCalled);
    this->manager.flush(StreamKey::TEST);
    ASSERT_TRUE(this->handlerCalled);
}

TEST_F(ManagerTest, unsubscribeShouldFlushTheStream)
{
    auto id = this->manager.subscribe(this->subscription);
    this->manager.emit(std::make_unique<TestEvent>());
    ASSERT_FALSE(this->handlerCalled);
    this->manager.unsubscribe(id);
    ASSERT_TRUE(this->handlerCalled);
}

TEST_F(ManagerTest, unsubscribeShouldNotFlushTheStream)
{
    auto id = this->manager.subscribe(this->subscription);
    this->manager.subscribe(this->subscription);
    this->manager.emit(std::make_unique<TestEvent>());
    this->manager.unsubscribe(id);
    ASSERT_FALSE(this->handlerCalled);
}
