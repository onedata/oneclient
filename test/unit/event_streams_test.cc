/**
 * @file event_streams_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "communication/communicator.h"
#include "eventCommunicator_mock.h"
#include "eventTestUtils.h"
#include "events/buffers/ioEventBuffer.h"
#include "events/buffers/voidEventBuffer.h"
#include "events/streams/ioEventStream.h"
#include "events/subscriptions/ioEventSubscription.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"
#include "scheduler_mock.h"

#include <vector>
#include <chrono>

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::communication;
using namespace one::client::events;
using namespace std::literals::chrono_literals;

template <class EventT> class IOEventStreamTest : public ::testing::Test {
public:
    IOEventStreamTest()
    {
        ctx = std::make_shared<Context>();
        ctx->setCommunicator(std::make_shared<Communicator>(
            1, "localhost", 80, false, communication::createConnection));
        sched = std::make_shared<NiceMock<MockScheduler>>();
        ctx->setScheduler(sched);
        evtComm = std::make_unique<NiceMock<MockEventCommunicator>>(ctx);
        evtStm = std::make_unique<IOEventStream<EventT>>(ctx, *evtComm);

        ON_CALL(*sched, post(_, _))
            .WillByDefault(WithArgs<1>(
                Invoke([](const std::function<void()> &task) { task(); })));
        ON_CALL(*evtComm, send(_))
            .WillByDefault(Invoke([this](const Event &evt) {
                this->evts.push_back(static_cast<const EventT &>(evt));
            }));
    }

protected:
    std::shared_ptr<Context> ctx;
    std::shared_ptr<NiceMock<MockScheduler>> sched;
    std::unique_ptr<NiceMock<MockEventCommunicator>> evtComm;
    std::unique_ptr<IOEventStream<EventT>> evtStm;
    std::vector<EventT> evts;
};

class ReadEventStreamTest : public IOEventStreamTest<ReadEvent> {
};

class WriteEventStreamTest : public IOEventStreamTest<WriteEvent> {
};

TEST_F(ReadEventStreamTest, forwardsEvents)
{
    evtStm->subscribe(IOEventSubscription<ReadEvent>{1, 1, 10s, 100});
    evtStm->push(ReadEvent{0, 10, "fileUuid"});
    EXPECT_EQ(1, evts.size());
    EXPECT_EQ("fileUuid", evts[0].fileUuid());
    EXPECT_EQ(10, evts[0].size());
    EXPECT_TRUE(blocks({{0, 10}}) == evts[0].blocks());
}

TEST_F(ReadEventStreamTest, addsSubscriptions)
{
    evtStm->subscribe(IOEventSubscription<ReadEvent>{1, 2, 10s, 100});
    evtStm->push(ReadEvent{0, 10, "fileUuid"});
    EXPECT_EQ(0, evts.size());
    evtStm->push(ReadEvent{10, 10, "fileUuid"});
    EXPECT_EQ(1, evts.size());
    evtStm->push(ReadEvent{0, 100, "fileUuid"});
    EXPECT_EQ(2, evts.size());
    evtStm->subscribe(IOEventSubscription<ReadEvent>{1, 1, 10s, 100});
    evtStm->push(ReadEvent{20, 10, "fileUuid"});
    EXPECT_EQ(3, evts.size());
    evtStm->push(ReadEvent{10, 10, "fileUuid"});
    EXPECT_EQ(4, evts.size());
}

TEST_F(ReadEventStreamTest, removesSubscriptions)
{
    const auto &sub1 =
        evtStm->subscribe(IOEventSubscription<ReadEvent>{1, 1, 10s, 100});
    const auto &sub2 =
        evtStm->subscribe(IOEventSubscription<ReadEvent>{1, 2, 10s, 100});
    evtStm->push(ReadEvent{0, 10, "fileUuid"});
    EXPECT_EQ(1, evts.size());
    sub1.second();
    evtStm->push(ReadEvent{10, 10, "fileUuid"});
    EXPECT_EQ(1, evts.size());
    evtStm->push(ReadEvent{20, 10, "fileUuid"});
    EXPECT_EQ(2, evts.size());
    sub2.second();
    evtStm->push(ReadEvent{0, 100, "fileUuid"});
    EXPECT_EQ(2, evts.size());
}

TEST_F(WriteEventStreamTest, forwardsEvents)
{
    evtStm->subscribe(IOEventSubscription<WriteEvent>{1, 1, 10s, 100});
    evtStm->push(WriteEvent{0, 10, "fileUuid1"});
    EXPECT_EQ(1, evts.size());
    EXPECT_EQ("fileUuid1", evts[0].fileUuid());
    EXPECT_EQ(10, evts[0].size());
    EXPECT_FALSE(evts[0].fileSize());
    EXPECT_TRUE(blocks({{0, 10}}) == evts[0].blocks());
    evtStm->push(TruncateEvent{10, "fileUuid2"});
    EXPECT_EQ(2, evts.size());
    EXPECT_EQ("fileUuid2", evts[1].fileUuid());
    EXPECT_EQ(0, evts[1].size());
    EXPECT_EQ(10, evts[1].fileSize().get());
    EXPECT_TRUE(evts[1].blocks().empty());
}

TEST_F(WriteEventStreamTest, addsSubscriptions)
{
    evtStm->subscribe(IOEventSubscription<WriteEvent>{1, 2, 10s, 100});
    evtStm->push(WriteEvent{0, 10, "fileUuid"});
    EXPECT_EQ(0, evts.size());
    evtStm->push(TruncateEvent{0, "fileUuid"});
    EXPECT_EQ(1, evts.size());
    evtStm->push(WriteEvent{0, 100, "fileUuid"});
    EXPECT_EQ(2, evts.size());
    evtStm->subscribe(IOEventSubscription<WriteEvent>{1, 1, 10s, 100});
    evtStm->push(WriteEvent{20, 10, "fileUuid"});
    EXPECT_EQ(3, evts.size());
    evtStm->push(TruncateEvent{10, "fileUuid"});
    EXPECT_EQ(4, evts.size());
}

TEST_F(WriteEventStreamTest, removesSubscriptions)
{
    const auto &sub1 =
        evtStm->subscribe(IOEventSubscription<WriteEvent>{1, 1, 10s, 100});
    const auto &sub2 =
        evtStm->subscribe(IOEventSubscription<WriteEvent>{1, 2, 10s, 100});
    evtStm->push(TruncateEvent{0, "fileUuid"});
    EXPECT_EQ(1, evts.size());
    sub1.second();
    evtStm->push(WriteEvent{0, 10, "fileUuid"});
    EXPECT_EQ(1, evts.size());
    evtStm->push(TruncateEvent{10, "fileUuid"});
    EXPECT_EQ(2, evts.size());
    sub2.second();
    evtStm->push(WriteEvent{0, 100, "fileUuid"});
    EXPECT_EQ(2, evts.size());
}
