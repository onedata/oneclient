/**
 * @file event_manager_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "communication/communicator.h"
#include "eventStream_mock.h"
#include "eventTestUtils.h"
#include "events/eventManager.h"
#include "events/eventStream.h"
#include "subscriptionRegistry_mock.h"

#include "messages.pb.h"

#include <gmock/gmock.h>

#include <memory>

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::client::events;
using namespace one::communication;

class ProxyEventManager : public EventManager {
public:
    ProxyEventManager(std::shared_ptr<Context> context)
        : EventManager{std::move(context)}
    {
    }

    void setReadEventStream(
        std::unique_ptr<MockEventStream<ReadEventStream>> stream)
    {
        m_readEventStream = std::move(stream);
    }

    void setWriteEventStream(
        std::unique_ptr<MockEventStream<WriteEventStream>> stream)
    {
        m_writeEventStream = std::move(stream);
    }

    void setSubscriptionRegistry(
        std::unique_ptr<MockSubscriptionRegistry> registry)
    {
        m_registry = std::move(registry);
    }
};

class EventManagerTest : public ::testing::Test {
public:
    EventManagerTest()
        : context{std::make_shared<Context>()}
    {
        context->setCommunicator(std::make_shared<Communicator>(
            1, "localhost", 80, false, communication::createConnection));
        streamManager = std::make_unique<communication::StreamManager>(
            context->communicator());
        eventManager = std::make_unique<ProxyEventManager>(context);
    }

protected:
    std::shared_ptr<Context> context;
    std::unique_ptr<communication::StreamManager> streamManager;
    std::unique_ptr<ProxyEventManager> eventManager;
};

TEST_F(EventManagerTest, handleShouldForwardReadEventSubscription)
{
    one::clproto::ServerMessage serverMsg{};
    auto subscriptionMsg = serverMsg.mutable_subscription();
    subscriptionMsg->mutable_read_subscription();

    auto readEventStream = std::make_unique<MockEventStream<ReadEventStream>>(
        streamManager->create());
    EXPECT_CALL(*readEventStream, subscribe(_)).Times(1);
    eventManager->setReadEventStream(std::move(readEventStream));

    eventManager->handle(*subscriptionMsg);
}

TEST_F(EventManagerTest, handleShouldForwardWriteEventSubscription)
{
    one::clproto::ServerMessage serverMsg{};
    auto subscriptionMsg = serverMsg.mutable_subscription();
    subscriptionMsg->mutable_write_subscription();

    auto writeEventStream = std::make_unique<MockEventStream<WriteEventStream>>(
        streamManager->create());
    EXPECT_CALL(*writeEventStream, subscribe(_)).Times(1);
    eventManager->setWriteEventStream(std::move(writeEventStream));

    eventManager->handle(*subscriptionMsg);
}

TEST_F(EventManagerTest, handleShouldForwardSubscriptionCancellation)
{
    one::clproto::ServerMessage message{};
    auto subscription = message.mutable_subscription_cancellation();

    auto registry = std::make_unique<MockSubscriptionRegistry>();
    EXPECT_CALL(*registry, removeSubscription(_)).Times(1);
    eventManager->setSubscriptionRegistry(std::move(registry));

    eventManager->handle(*subscription);
}
