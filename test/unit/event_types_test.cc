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
    ReadEventTest() { evt = std::move(ReadEvent{0, 10, "fileUuid1"}); }
};

class WriteEventTest : public EventTest<WriteEvent> {
public:
    WriteEventTest() { evt = std::move(WriteEvent{0, 10, "fileUuid1"}); }
};

class TruncateEventTest : public EventTest<TruncateEvent> {
public:
    TruncateEventTest() { evt = std::move(TruncateEvent{10, "fileUuid1"}); }
};

TYPED_TEST(EventTest, eventShouldHaveAggregationIdentityElement)
{
    EXPECT_EQ(0, this->evt.counter());
    EXPECT_EQ("", this->evt.fileUuid());
    EXPECT_EQ(0, this->evt.size());
    EXPECT_TRUE(this->evt.blocks().empty());
}

TEST_F(ReadEventTest, eventShouldAggregateOtherEventsAssociatedWithTheSameFile)
{
    this->evt += ReadEvent{20, 10, "fileUuid1"};
    EXPECT_EQ(2, this->evt.counter());
    EXPECT_EQ("fileUuid1", this->evt.fileUuid());
    EXPECT_EQ(20, this->evt.size());
    EXPECT_TRUE(blocks({{0, 10}, {20, 30}}) == this->evt.blocks());

    this->evt += ReadEvent{5, 20, "fileUuid1"};
    EXPECT_EQ(3, this->evt.counter());
    EXPECT_EQ("fileUuid1", this->evt.fileUuid());
    EXPECT_EQ(40, this->evt.size());
    EXPECT_TRUE(blocks({{0, 30}}) == this->evt.blocks());
}

TEST_F(ReadEventTest, eventShouldCompareWithOtherEvents)
{
    ReadEvent evt0{0, 10, "fileUuid0"};
    ReadEvent evt1{0, 10, "fileUuid1"};
    ReadEvent evt2{0, 10, "fileUuid2"};

    EXPECT_FALSE(this->evt < evt0);
    EXPECT_TRUE(evt0 < this->evt);

    EXPECT_FALSE(this->evt < evt1);
    EXPECT_FALSE(evt1 < this->evt);

    EXPECT_TRUE(this->evt < evt2);
    EXPECT_FALSE(evt2 < this->evt);
}

TEST_F(ReadEventTest, eventShouldSerialize)
{
    one::clproto::ClientMessage cliMsg{};
    auto evtMsg = cliMsg.mutable_event();
    evtMsg->set_counter(1);
    auto readEvtMsg = evtMsg->mutable_read_event();
    readEvtMsg->set_file_uuid("fileUuid1");
    readEvtMsg->set_size(10);
    auto blockMsg = readEvtMsg->add_blocks();
    blockMsg->set_offset(0);
    blockMsg->set_size(10);
    blockMsg->set_file_id("");
    blockMsg->set_storage_id("");
    EXPECT_EQ(cliMsg.SerializeAsString(), evt.serialize()->SerializeAsString());
}

TEST_F(WriteEventTest, eventShouldAggregateOtherEventsAssociatedWithTheSameFile)
{
    this->evt += WriteEvent{20, 10, "fileUuid1"};
    EXPECT_EQ(2, this->evt.counter());
    EXPECT_EQ("fileUuid1", this->evt.fileUuid());
    EXPECT_EQ(20, this->evt.size());
    EXPECT_FALSE(this->evt.fileSize());
    EXPECT_TRUE(blocks({{0, 10}, {20, 30}}) == this->evt.blocks());

    this->evt += WriteEvent{5, 20, "fileUuid1"};
    EXPECT_EQ(3, this->evt.counter());
    EXPECT_EQ("fileUuid1", this->evt.fileUuid());
    EXPECT_EQ(40, this->evt.size());
    EXPECT_FALSE(this->evt.fileSize());
    EXPECT_TRUE(blocks({{0, 30}}) == this->evt.blocks());
}

TEST_F(WriteEventTest, eventShouldCompareWithOtherEvents)
{
    WriteEvent evt0{0, 10, "fileUuid0"};
    WriteEvent evt1{0, 10, "fileUuid1"};
    WriteEvent evt2{0, 10, "fileUuid2"};

    EXPECT_FALSE(this->evt < evt0);
    EXPECT_TRUE(evt0 < this->evt);

    EXPECT_FALSE(this->evt < evt1);
    EXPECT_FALSE(evt1 < this->evt);

    EXPECT_TRUE(this->evt < evt2);
    EXPECT_FALSE(evt2 < this->evt);
}

TEST_F(WriteEventTest, eventShouldSerialize)
{
    one::clproto::ClientMessage cliMsg{};
    auto evtMsg = cliMsg.mutable_event();
    evtMsg->set_counter(1);
    auto writeEvtMsg = evtMsg->mutable_write_event();
    writeEvtMsg->set_file_uuid("fileUuid1");
    writeEvtMsg->set_size(10);
    auto blockMsg = writeEvtMsg->add_blocks();
    blockMsg->set_offset(0);
    blockMsg->set_size(10);
    blockMsg->set_storage_id("");
    blockMsg->set_file_id("");
    EXPECT_EQ(cliMsg.SerializeAsString(), evt.serialize()->SerializeAsString());
}

TEST_F(
    TruncateEventTest, eventShouldAggregateOtherEventsAssociatedWithTheSameFile)
{
    this->evt += TruncateEvent{30, "fileUuid1"};
    EXPECT_EQ(2, this->evt.counter());
    EXPECT_EQ("fileUuid1", this->evt.fileUuid());
    EXPECT_EQ(30, this->evt.fileSize().get());
    EXPECT_TRUE(this->evt.blocks().empty());

    this->evt += TruncateEvent{0, "fileUuid1"};
    EXPECT_EQ(3, this->evt.counter());
    EXPECT_EQ("fileUuid1", this->evt.fileUuid());
    EXPECT_EQ(0, this->evt.fileSize().get());
    EXPECT_TRUE(this->evt.blocks().empty());
}

TEST_F(TruncateEventTest, eventShouldCompareWithOtherEvents)
{
    TruncateEvent evt0{10, "fileUuid0"};
    TruncateEvent evt1{10, "fileUuid1"};
    TruncateEvent evt2{10, "fileUuid2"};

    EXPECT_FALSE(this->evt < evt0);
    EXPECT_TRUE(evt0 < this->evt);

    EXPECT_FALSE(this->evt < evt1);
    EXPECT_FALSE(evt1 < this->evt);

    EXPECT_TRUE(this->evt < evt2);
    EXPECT_FALSE(evt2 < this->evt);
}

TEST_F(TruncateEventTest, eventShouldSerialize)
{
    one::clproto::ClientMessage cliMsg{};
    auto evtMsg = cliMsg.mutable_event();
    evtMsg->set_counter(1);
    auto writeEvtMsg = evtMsg->mutable_write_event();
    writeEvtMsg->set_file_uuid("fileUuid1");
    writeEvtMsg->set_size(0);
    writeEvtMsg->set_file_size(10);
    EXPECT_EQ(cliMsg.SerializeAsString(), evt.serialize()->SerializeAsString());
}
