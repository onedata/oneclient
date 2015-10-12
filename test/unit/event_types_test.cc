/**
 * @file event_types_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"

#include "messages.pb.h"

#include <gtest/gtest.h>

using namespace one::client::events;

template <class EventT> class EventTest : public ::testing::Test {
protected:
    EventT evt;
};

TYPED_TEST_CASE(EventTest, AllEventTypes);

class ReadEventTest : public EventTest<ReadEvent> {
public:
    ReadEventTest() { evt = std::move(ReadEvent{"fileId1", 0, 10}); }
};

class WriteEventTest : public EventTest<WriteEvent> {
public:
    WriteEventTest() { evt = std::move(WriteEvent{"fileId1", 0, 10, 10}); }
};

class TruncateEventTest : public EventTest<TruncateEvent> {
public:
    TruncateEventTest() { evt = std::move(TruncateEvent{"fileId1", 10}); }
};

TYPED_TEST(EventTest, aggregationIdentityElement)
{
    EXPECT_EQ(0, this->evt.counter());
    EXPECT_EQ("", this->evt.fileId());
    EXPECT_EQ(0, this->evt.size());
    EXPECT_TRUE(this->evt.blocks().empty());
}

TEST_F(ReadEventTest, aggregatesReadEventsAssociatedWithTheSameFile)
{
    this->evt += ReadEvent{"fileId1", 20, 10};
    EXPECT_EQ(2, this->evt.counter());
    EXPECT_EQ("fileId1", this->evt.fileId());
    EXPECT_EQ(20, this->evt.size());
    EXPECT_TRUE(blocks({{0, 10}, {20, 30}}) == this->evt.blocks());

    this->evt += ReadEvent{"fileId1", 5, 20};
    EXPECT_EQ(3, this->evt.counter());
    EXPECT_EQ("fileId1", this->evt.fileId());
    EXPECT_EQ(40, this->evt.size());
    EXPECT_TRUE(blocks({{0, 30}}) == this->evt.blocks());
}

TEST_F(ReadEventTest, comparesWithOtherReadEvents)
{
    ReadEvent evt0{"fileId0", 0, 10};
    ReadEvent evt1{"fileId1", 0, 10};
    ReadEvent evt2{"fileId2", 0, 10};

    EXPECT_FALSE(this->evt < evt0);
    EXPECT_TRUE(evt0 < this->evt);

    EXPECT_FALSE(this->evt < evt1);
    EXPECT_FALSE(evt1 < this->evt);

    EXPECT_TRUE(this->evt < evt2);
    EXPECT_FALSE(evt2 < this->evt);
}

TEST_F(ReadEventTest, serializes)
{
    one::clproto::ClientMessage cliMsg{};
    auto evtMsg = cliMsg.mutable_event();
    auto readEvtMsg = evtMsg->mutable_read_event();
    readEvtMsg->set_counter(1);
    readEvtMsg->set_file_id("fileId1");
    readEvtMsg->set_size(10);
    auto blockMsg = readEvtMsg->add_blocks();
    blockMsg->set_offset(0);
    blockMsg->set_size(10);
    EXPECT_EQ(cliMsg.SerializeAsString(), evt.serialize()->SerializeAsString());
}

TEST_F(WriteEventTest, aggregatesWriteEventsAssociatedWithTheSameFile)
{
    this->evt += WriteEvent{"fileId1", 20, 10, 30};
    EXPECT_EQ(2, this->evt.counter());
    EXPECT_EQ("fileId1", this->evt.fileId());
    EXPECT_EQ(20, this->evt.size());
    EXPECT_EQ(30, this->evt.fileSize());
    EXPECT_TRUE(blocks({{0, 10}, {20, 30}}) == this->evt.blocks());

    this->evt += WriteEvent{"fileId1", 5, 20, 30};
    EXPECT_EQ(3, this->evt.counter());
    EXPECT_EQ("fileId1", this->evt.fileId());
    EXPECT_EQ(40, this->evt.size());
    EXPECT_EQ(30, this->evt.fileSize());
    EXPECT_TRUE(blocks({{0, 30}}) == this->evt.blocks());
}

TEST_F(WriteEventTest, comparesWithOtherWriteEvents)
{
    WriteEvent evt0{"fileId0", 0, 10, 10};
    WriteEvent evt1{"fileId1", 0, 10, 10};
    WriteEvent evt2{"fileId2", 0, 10, 10};

    EXPECT_FALSE(this->evt < evt0);
    EXPECT_TRUE(evt0 < this->evt);

    EXPECT_FALSE(this->evt < evt1);
    EXPECT_FALSE(evt1 < this->evt);

    EXPECT_TRUE(this->evt < evt2);
    EXPECT_FALSE(evt2 < this->evt);
}

TEST_F(WriteEventTest, serializes)
{
    one::clproto::ClientMessage cliMsg{};
    auto evtMsg = cliMsg.mutable_event();
    auto writeEvtMsg = evtMsg->mutable_write_event();
    writeEvtMsg->set_counter(1);
    writeEvtMsg->set_file_id("fileId1");
    writeEvtMsg->set_size(10);
    writeEvtMsg->set_file_size(10);
    auto blockMsg = writeEvtMsg->add_blocks();
    blockMsg->set_offset(0);
    blockMsg->set_size(10);
    EXPECT_EQ(cliMsg.SerializeAsString(), evt.serialize()->SerializeAsString());
}

TEST_F(TruncateEventTest, aggregatesTruncateEventsAssociatedWithTheSameFile)
{
    this->evt += TruncateEvent{"fileId1", 30};
    EXPECT_EQ(2, this->evt.counter());
    EXPECT_EQ("fileId1", this->evt.fileId());
    EXPECT_EQ(30, this->evt.fileSize());
    EXPECT_TRUE(this->evt.blocks().empty());

    this->evt += TruncateEvent{"fileId1", 0};
    EXPECT_EQ(3, this->evt.counter());
    EXPECT_EQ("fileId1", this->evt.fileId());
    EXPECT_EQ(0, this->evt.fileSize());
    EXPECT_TRUE(this->evt.blocks().empty());
}

TEST_F(TruncateEventTest, comparesWithOtherTruncateEvents)
{
    TruncateEvent evt0{"fileId0", 10};
    TruncateEvent evt1{"fileId1", 10};
    TruncateEvent evt2{"fileId2", 10};

    EXPECT_FALSE(this->evt < evt0);
    EXPECT_TRUE(evt0 < this->evt);

    EXPECT_FALSE(this->evt < evt1);
    EXPECT_FALSE(evt1 < this->evt);

    EXPECT_TRUE(this->evt < evt2);
    EXPECT_FALSE(evt2 < this->evt);
}

TEST_F(TruncateEventTest, serializes)
{
    one::clproto::ClientMessage cliMsg{};
    auto evtMsg = cliMsg.mutable_event();
    auto writeEvtMsg = evtMsg->mutable_write_event();
    writeEvtMsg->set_counter(1);
    writeEvtMsg->set_file_id("fileId1");
    writeEvtMsg->set_size(0);
    writeEvtMsg->set_file_size(10);
    EXPECT_EQ(cliMsg.SerializeAsString(), evt.serialize()->SerializeAsString());
}
