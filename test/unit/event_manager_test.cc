/**
 * @file event_manager_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "communication/communicator.h"
#include "eventManager_proxy.h"
#include "events/eventCommunicator.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"
#include "ioEventStream_mock.h"
#include "scheduler_mock.h"
#include "subscriptionRegistry_mock.h"

#include "messages.pb.h"

#include <gtest/gtest.h>

#include <memory>

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::communication;
using namespace one::client::events;

class EventManagerTest : public ::testing::Test {
public:
    EventManagerTest()
    {
        ctx = std::make_shared<Context>();
        ctx->setCommunicator(std::make_shared<Communicator>(
            1, "localhost", 80, false, communication::createConnection));
        sched = std::make_shared<NiceMock<MockScheduler>>();
        ctx->setScheduler(sched);
        evtComm = std::make_unique<EventCommunicator>(ctx);

        ON_CALL(*sched, post(_, _))
            .WillByDefault(WithArgs<1>(
                Invoke([](const std::function<void()> &task) { task(); })));

        evtMan = std::make_unique<ProxyEventManager>(ctx);
    }

protected:
    std::shared_ptr<Context> ctx;
    std::shared_ptr<NiceMock<MockScheduler>> sched;
    std::unique_ptr<EventCommunicator> evtComm;
    std::unique_ptr<ProxyEventManager> evtMan;
};

TEST_F(EventManagerTest, emitsReadEvent)
{
    auto readEvtStm = std::make_unique<MockReadEventStream>(ctx, *evtComm);
    EXPECT_CALL(*readEvtStm, push(_)).Times(1);
    evtMan->setReadEventStream(std::move(readEvtStm));
    evtMan->emitReadEvent(0, 10, "fileUuid");
}

TEST_F(EventManagerTest, emitsWriteEvent)
{
    auto writeEvtStm = std::make_unique<MockWriteEventStream>(ctx, *evtComm);
    EXPECT_CALL(*writeEvtStm, push(_)).Times(1);
    evtMan->setWriteEventStream(std::move(writeEvtStm));
    evtMan->emitWriteEvent(0, 10, "fileUuid", "storageId", "fileUuid");
}

TEST_F(EventManagerTest, emitsTrucateEvent)
{
    auto writeEvtStm = std::make_unique<MockWriteEventStream>(ctx, *evtComm);
    EXPECT_CALL(*writeEvtStm, push(_)).Times(1);
    evtMan->setWriteEventStream(std::move(writeEvtStm));
    evtMan->emitTruncateEvent(10, "fileUuid");
}

TEST_F(EventManagerTest, handlesReadEventSubscriptionMessage)
{
    one::clproto::ServerMessage msg{};
    msg.mutable_event_subscription()->mutable_read_event_subscription();

    auto readEvtStm = std::make_unique<MockReadEventStream>(ctx, *evtComm);
    EXPECT_CALL(*readEvtStm, subscribe(_)).Times(1);
    evtMan->setReadEventStream(std::move(readEvtStm));

    auto subReg = std::make_unique<MockSubscriptionRegistry>(ctx);
    EXPECT_CALL(*subReg, add(_)).Times(1);
    evtMan->setSubscriptionRegistry(std::move(subReg));

    evtMan->handleServerMessage(msg);
}

TEST_F(EventManagerTest, handlesWriteEventSubscriptionMessage)
{
    one::clproto::ServerMessage msg{};
    msg.mutable_event_subscription()->mutable_write_event_subscription();

    auto writeEvtStm = std::make_unique<MockWriteEventStream>(ctx, *evtComm);
    EXPECT_CALL(*writeEvtStm, subscribe(_)).Times(1);
    evtMan->setWriteEventStream(std::move(writeEvtStm));

    auto subReg = std::make_unique<MockSubscriptionRegistry>(ctx);
    EXPECT_CALL(*subReg, add(_)).Times(1);
    evtMan->setSubscriptionRegistry(std::move(subReg));

    evtMan->handleServerMessage(msg);
}

TEST_F(EventManagerTest, handlesEventSubscriptionCancellationMessage)
{
    one::clproto::ServerMessage msg{};
    msg.mutable_event_subscription()->mutable_event_subscription_cancellation();

    auto subReg = std::make_unique<MockSubscriptionRegistry>(ctx);
    EXPECT_CALL(*subReg, remove(_)).Times(1);
    evtMan->setSubscriptionRegistry(std::move(subReg));

    evtMan->handleServerMessage(msg);
}
