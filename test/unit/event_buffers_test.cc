/**
 * @file event_buffers_test.cc
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
#include "events/subscriptions/ioEventSubscription.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"

#include <vector>
#include <memory>
#include <chrono>
#include <algorithm>
#include <functional>

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::communication;
using namespace one::client::events;
using namespace std::literals::chrono_literals;

template <class EventT> class VoidEventBufferTest : public ::testing::Test {
public:
    VoidEventBufferTest() { buf = std::make_unique<VoidEventBuffer<EventT>>(); }

protected:
    std::unique_ptr<EventBuffer<EventT>> buf;
};

TYPED_TEST_CASE(VoidEventBufferTest, AllEventTypes);

template <class EventT> class IOEventBufferTest : public ::testing::Test {
public:
    IOEventBufferTest()
        : evtSub{1, 10, 10s, 100}
    {
        ctx = std::make_shared<Context>();
        ctx->setCommunicator(std::make_shared<Communicator>(
            1, "localhost", 80, false, communication::createConnection));
        evtComm = std::make_unique<NiceMock<MockEventCommunicator>>(ctx);
        buf = std::make_unique<IOEventBuffer<EventT>>(*evtComm, evtSub);

        ON_CALL(*evtComm, send(_))
            .WillByDefault(Invoke([this](const Event &evt) {
                this->evts.push_back(static_cast<const EventT &>(evt));
            }));
    }

protected:
    IOEventSubscription<EventT> evtSub;
    std::shared_ptr<Context> ctx;
    std::unique_ptr<NiceMock<MockEventCommunicator>> evtComm;
    std::unique_ptr<EventBuffer<EventT>> buf;
    std::vector<EventT> evts;
};

TYPED_TEST_CASE(IOEventBufferTest, AllEventTypes);

class ReadEventBufferTest : public IOEventBufferTest<ReadEvent> {
};

class WriteEventBufferTest : public IOEventBufferTest<WriteEvent> {
};

class TruncateEventBufferTest : public IOEventBufferTest<TruncateEvent> {
};

TYPED_TEST(VoidEventBufferTest, doesNotForwardEvents)
{
    for (int i = 0; i < 10; ++i)
        EXPECT_FALSE(this->buf->push(TypeParam{}));
}

TYPED_TEST(VoidEventBufferTest, doesNotForwardEventsOnTryClear)
{
    EXPECT_FALSE(this->buf->try_clear());
}

TYPED_TEST(IOEventBufferTest, forwardsEventsOnClear)
{
    EXPECT_FALSE(this->buf->push(TypeParam{}));
    this->buf->clear();
    EXPECT_EQ(1, this->evts.size());
}

TYPED_TEST(IOEventBufferTest,
    doesNotforwardEventsOnTryClearWhenSubscriptionNotSatisfied)
{
    EXPECT_FALSE(this->buf->push(TypeParam{}));
    EXPECT_FALSE(this->buf->try_clear());
    EXPECT_TRUE(this->evts.empty());
}

TEST_F(ReadEventBufferTest, doesNotForwardEventsWhenThresholdsAreNotExceeded)
{
    for (int i = 0; i < 5; ++i)
        EXPECT_FALSE(buf->push(ReadEvent{"fileId", 0, 10}));
    EXPECT_TRUE(evts.empty());
}

TEST_F(ReadEventBufferTest, forwardsEventsWhenSizeThresholdMet)
{
    EXPECT_TRUE(buf->push(ReadEvent{"fileId", 0, 100}));
    EXPECT_EQ(1, evts.size());
}

TEST_F(ReadEventBufferTest, forwardsEventsWhenSizeThresholdExceeded)
{
    EXPECT_TRUE(buf->push(ReadEvent{"fileId", 0, 101}));
    EXPECT_EQ(1, evts.size());
}

TEST_F(ReadEventBufferTest, forwardsEventsWhenCounterThresholdMet)
{
    for (int i = 0; i < 9; ++i)
        EXPECT_FALSE(buf->push(ReadEvent{"fileId", 0, 10}));
    EXPECT_TRUE(buf->push(ReadEvent{"fileId", 0, 10}));
    EXPECT_EQ(1, evts.size());
}

TEST_F(ReadEventBufferTest, forwardsMultipleEvents)
{
    for (int i = 0; i < 10; ++i) {
        for (int j = 0; j < 9; ++j)
            EXPECT_FALSE(buf->push(ReadEvent{"fileId", 0, 10}));
        EXPECT_TRUE(buf->push(ReadEvent{"fileId", 0, 10}));
        EXPECT_EQ(i + 1, evts.size());
    }
}

TEST_F(ReadEventBufferTest, aggregatesEventsByFileId)
{
    std::vector<std::string> fileIds{"1", "2", "3", "4", "5"};
    for (off_t i = 0; i < 2; ++i) {
        for (const auto &fileId : fileIds)
            buf->push(ReadEvent{fileId, i, 1});
    }
    EXPECT_EQ(fileIds.size(), evts.size());
    std::sort(evts.begin(), evts.end());
    for (unsigned int i = 0; i < fileIds.size(); ++i) {
        EXPECT_EQ(fileIds[i], evts[i].fileId());
        EXPECT_EQ(2, evts[i].counter());
        EXPECT_EQ(2, evts[i].size());
        EXPECT_TRUE(blocks({{0, 2}}) == evts[i].blocks());
    }
}

TEST_F(ReadEventBufferTest, aggregatesEventsForFileId)
{
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 0, 2}));
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 3, 2}));
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 1, 5}));
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 10, 3}));
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 12, 2}));
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 15, 3}));
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 15, 4}));
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 15, 5}));
    EXPECT_FALSE(buf->push(ReadEvent{"fileId", 20, 5}));
    EXPECT_TRUE(buf->push(ReadEvent{"fileId", 22, 3}));

    EXPECT_EQ(1, evts.size());
    EXPECT_EQ("fileId", evts[0].fileId());
    EXPECT_EQ(10, evts[0].counter());
    EXPECT_EQ(34, evts[0].size());
    EXPECT_TRUE(blocks({{0, 6}, {10, 14}, {15, 25}}) == evts[0].blocks());
}

TEST_F(WriteEventBufferTest, aggregatesEventsForFileId)
{
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 0, 2, 2}));
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 3, 2, 5}));
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 1, 5, 6}));
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 10, 3, 13}));
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 12, 2, 14}));
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 15, 3, 18}));
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 15, 4, 19}));
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 15, 5, 20}));
    EXPECT_FALSE(buf->push(WriteEvent{"fileId", 20, 5, 25}));
    EXPECT_TRUE(buf->push(WriteEvent{"fileId", 22, 3, 25}));

    EXPECT_EQ(1, evts.size());
    EXPECT_EQ("fileId", evts[0].fileId());
    EXPECT_EQ(10, evts[0].counter());
    EXPECT_EQ(34, evts[0].size());
    EXPECT_EQ(25, evts[0].fileSize());
    EXPECT_TRUE(blocks({{0, 6}, {10, 14}, {15, 25}}) == evts[0].blocks());
}

TEST_F(TruncateEventBufferTest, aggregatesEventsForFileId)
{
    EXPECT_FALSE(buf->push(TruncateEvent{"fileId", 5}));
    EXPECT_FALSE(buf->push(TruncateEvent{"fileId", 10}));
    buf->clear();
    EXPECT_EQ(1, evts.size());
    EXPECT_EQ(2, evts[0].counter());
    EXPECT_EQ(10, evts[0].fileSize());

    EXPECT_FALSE(buf->push(TruncateEvent{"fileId", 15}));
    EXPECT_FALSE(buf->push(TruncateEvent{"fileId", 0}));
    buf->clear();
    EXPECT_EQ(2, evts.size());
    EXPECT_EQ(2, evts[0].counter());
    EXPECT_EQ(0, evts[1].fileSize());
}
