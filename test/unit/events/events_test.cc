/**
 * @file events_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "eventTestUtils.h"

#include "messages.pb.h"

using namespace one::client::events;

template <class EventT> class EventTest : public ::testing::Test {
protected:
    std::unique_ptr<EventT> event;
};

class ReadEventTest : public EventTest<ReadEvent> {
public:
    ReadEventTest() { event = readEventPtr(0, 10, "fileUuid1"); }
};

class WriteEventTest : public EventTest<WriteEvent> {
public:
    WriteEventTest() { event = writeEventPtr(0, 10, "fileUuid1"); }
};

TEST_F(ReadEventTest, aggregateShouldMergeEvents)
{
    event->aggregate(readEventPtr(20, 10, "fileUuid1"));
    EXPECT_EQ(2, event->counter());
    EXPECT_EQ("fileUuid1", event->fileUuid());
    EXPECT_EQ(20, event->size());
    EXPECT_TRUE(blocks({{0, 10}, {20, 30}}) == event->blocks());

    event->aggregate(readEventPtr(5, 20, "fileUuid1"));
    EXPECT_EQ(3, event->counter());
    EXPECT_EQ("fileUuid1", event->fileUuid());
    EXPECT_EQ(40, event->size());
    EXPECT_TRUE(blocks({{0, 30}}) == event->blocks());
}

TEST_F(ReadEventTest, serializeShouldCreateProtocolMessage)
{
    one::clproto::ClientMessage clientMsg{};
    auto eventMsg = clientMsg.mutable_event();
    eventMsg->set_counter(1);
    auto readEventMsg = eventMsg->mutable_read_event();
    readEventMsg->set_file_uuid("fileUuid1");
    readEventMsg->set_size(10);
    auto blockMsg = readEventMsg->add_blocks();
    blockMsg->set_offset(0);
    blockMsg->set_size(10);
    blockMsg->set_file_id("");
    blockMsg->set_storage_id("");
    EXPECT_EQ(
        clientMsg.SerializeAsString(), event->serialize()->SerializeAsString());
}

TEST_F(WriteEventTest, aggregateShouldMergeEvents)
{
    event->aggregate(writeEventPtr(20, 10, "fileUuid1"));
    EXPECT_EQ(2, event->counter());
    EXPECT_EQ("fileUuid1", event->fileUuid());
    EXPECT_EQ(20, event->size());
    EXPECT_FALSE(event->fileSize());
    EXPECT_TRUE(blocks({{0, 10}, {20, 30}}) == event->blocks());

    event->aggregate(writeEventPtr(5, 20, "fileUuid1"));
    EXPECT_EQ(3, event->counter());
    EXPECT_EQ("fileUuid1", event->fileUuid());
    EXPECT_EQ(40, event->size());
    EXPECT_FALSE(event->fileSize());
    EXPECT_TRUE(blocks({{0, 30}}) == event->blocks());
}

TEST_F(WriteEventTest, serializeShouldCreateProtocolMessage)
{
    one::clproto::ClientMessage clientMsg{};
    auto eventMsg = clientMsg.mutable_event();
    eventMsg->set_counter(1);
    auto writeEventMsg = eventMsg->mutable_write_event();
    writeEventMsg->set_file_uuid("fileUuid1");
    writeEventMsg->set_size(10);
    auto blockMsg = writeEventMsg->add_blocks();
    blockMsg->set_offset(0);
    blockMsg->set_size(10);
    blockMsg->set_storage_id("");
    blockMsg->set_file_id("");
    EXPECT_EQ(
        clientMsg.SerializeAsString(), event->serialize()->SerializeAsString());
}
