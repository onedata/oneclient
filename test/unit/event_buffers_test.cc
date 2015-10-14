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

TYPED_TEST(VoidEventBufferTest, bufferShouldNotForwardEvents)
{
    for (int i = 0; i < 10; ++i)
        EXPECT_FALSE(this->buf->push(TypeParam{}));
}

TYPED_TEST(VoidEventBufferTest, bufferShouldNotForwardEventsOnTryClear)
{
    EXPECT_FALSE(this->buf->try_clear());
}

TYPED_TEST(IOEventBufferTest, bufferShouldForwardEventsOnClear)
{
    EXPECT_FALSE(this->buf->push(TypeParam{}));
    this->buf->clear();
    EXPECT_EQ(1, this->evts.size());
}

TYPED_TEST(IOEventBufferTest,
    bufferShouldNotForwardEventsOnTryClearWhenSubscriptionIsNotSatisfied)
{
    EXPECT_FALSE(this->buf->push(TypeParam{}));
    EXPECT_FALSE(this->buf->try_clear());
    EXPECT_TRUE(this->evts.empty());
}

TEST_F(ReadEventBufferTest,
    bufferShouldNotForwardEventsWhenThresholdsAreNotExceeded)
{
    for (int i = 0; i < 5; ++i)
        EXPECT_FALSE(buf->push(ReadEvent{0, 10, "fileUuid"}));
    EXPECT_TRUE(evts.empty());
}

TEST_F(ReadEventBufferTest, bufferShouldForwardEventsWhenSizeThresholdMet)
{
    EXPECT_TRUE(buf->push(ReadEvent{0, 100, "fileUuid"}));
    EXPECT_EQ(1, evts.size());
}

TEST_F(ReadEventBufferTest, bufferShouldForwardEventsWhenSizeThresholdExceeded)
{
    EXPECT_TRUE(buf->push(ReadEvent{0, 101, "fileUuid"}));
    EXPECT_EQ(1, evts.size());
}

TEST_F(ReadEventBufferTest, bufferShouldForwardEventsWhenCounterThresholdMet)
{
    for (int i = 0; i < 9; ++i)
        EXPECT_FALSE(buf->push(ReadEvent{0, 10, "fileUuid"}));
    EXPECT_TRUE(buf->push(ReadEvent{0, 10, "fileUuid"}));
    EXPECT_EQ(1, evts.size());
}

TEST_F(ReadEventBufferTest, bufferShouldForwardMultipleEvents)
{
    for (int i = 0; i < 10; ++i) {
        for (int j = 0; j < 9; ++j)
            EXPECT_FALSE(buf->push(ReadEvent{0, 10, "fileUuid"}));
        EXPECT_TRUE(buf->push(ReadEvent{0, 10, "fileUuid"}));
        EXPECT_EQ(i + 1, evts.size());
    }
}

TEST_F(ReadEventBufferTest, bufferShouldAggregateEventsByFileUuid)
{
    std::vector<std::string> fileUuids{"1", "2", "3", "4", "5"};
    for (off_t i = 0; i < 2; ++i) {
        for (const auto &fileUuid : fileUuids)
            buf->push(ReadEvent{i, 1, fileUuid});
    }
    EXPECT_EQ(fileUuids.size(), evts.size());
    std::sort(evts.begin(), evts.end());
    for (unsigned int i = 0; i < fileUuids.size(); ++i) {
        EXPECT_EQ(fileUuids[i], evts[i].fileUuid());
        EXPECT_EQ(2, evts[i].counter());
        EXPECT_EQ(2, evts[i].size());
        EXPECT_TRUE(blocks({{0, 2}}) == evts[i].blocks());
    }
}

TEST_F(ReadEventBufferTest, bufferShouldAggregateEventsForTheSameFileUuid)
{
    EXPECT_FALSE(buf->push(ReadEvent{0, 2, "fileUuid"}));
    EXPECT_FALSE(buf->push(ReadEvent{3, 2, "fileUuid"}));
    EXPECT_FALSE(buf->push(ReadEvent{1, 5, "fileUuid"}));
    EXPECT_FALSE(buf->push(ReadEvent{10, 3, "fileUuid"}));
    EXPECT_FALSE(buf->push(ReadEvent{12, 2, "fileUuid"}));
    EXPECT_FALSE(buf->push(ReadEvent{15, 3, "fileUuid"}));
    EXPECT_FALSE(buf->push(ReadEvent{15, 4, "fileUuid"}));
    EXPECT_FALSE(buf->push(ReadEvent{15, 5, "fileUuid"}));
    EXPECT_FALSE(buf->push(ReadEvent{20, 5, "fileUuid"}));
    EXPECT_TRUE(buf->push(ReadEvent{22, 3, "fileUuid"}));

    EXPECT_EQ(1, evts.size());
    EXPECT_EQ("fileUuid", evts[0].fileUuid());
    EXPECT_EQ(10, evts[0].counter());
    EXPECT_EQ(34, evts[0].size());
    EXPECT_TRUE(blocks({{0, 6}, {10, 14}, {15, 25}}) == evts[0].blocks());
}

TEST_F(WriteEventBufferTest, bufferShouldAggregateEventsForTheSameFileUuid)
{
    EXPECT_FALSE(buf->push(WriteEvent{0, 2, "fileUuid"}));
    EXPECT_FALSE(buf->push(WriteEvent{3, 2, "fileUuid"}));
    EXPECT_FALSE(buf->push(WriteEvent{1, 5, "fileUuid"}));
    EXPECT_FALSE(buf->push(WriteEvent{10, 3, "fileUuid"}));
    EXPECT_FALSE(buf->push(WriteEvent{12, 2, "fileUuid"}));
    EXPECT_FALSE(buf->push(WriteEvent{15, 3, "fileUuid"}));
    EXPECT_FALSE(buf->push(WriteEvent{15, 4, "fileUuid"}));
    EXPECT_FALSE(buf->push(WriteEvent{15, 5, "fileUuid"}));
    EXPECT_FALSE(buf->push(WriteEvent{20, 5, "fileUuid"}));
    EXPECT_TRUE(buf->push(WriteEvent{22, 3, "fileUuid"}));

    EXPECT_EQ(1, evts.size());
    EXPECT_EQ("fileUuid", evts[0].fileUuid());
    EXPECT_EQ(10, evts[0].counter());
    EXPECT_EQ(34, evts[0].size());
    EXPECT_FALSE(evts[0].fileSize());
    EXPECT_TRUE(blocks({{0, 6}, {10, 14}, {15, 25}}) == evts[0].blocks());
}

TEST_F(TruncateEventBufferTest, bufferShouldAggregateEventsForTheSameFileUuid)
{
    EXPECT_FALSE(buf->push(TruncateEvent{5, "fileUuid"}));
    EXPECT_FALSE(buf->push(TruncateEvent{10, "fileUuid"}));
    buf->clear();
    EXPECT_EQ(1, evts.size());
    EXPECT_EQ(2, evts[0].counter());
    EXPECT_EQ(10, evts[0].fileSize().get());

    EXPECT_FALSE(buf->push(TruncateEvent{15, "fileUuid"}));
    EXPECT_FALSE(buf->push(TruncateEvent{0, "fileUuid"}));
    buf->clear();
    EXPECT_EQ(2, evts.size());
    EXPECT_EQ(2, evts[0].counter());
    EXPECT_EQ(0, evts[1].fileSize().get());
}
