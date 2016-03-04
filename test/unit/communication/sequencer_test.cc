/**
 * @file sequencer_test.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/declarations.h"
#include "communication/layers/sequencer.h"
#include "scheduler_mock.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <algorithm>
#include <functional>
#include <memory>
#include <numeric>
#include <system_error>
#include <vector>

using namespace one;
using namespace one::communication;
using namespace ::testing;

inline std::unique_ptr<clproto::ServerMessage> streamMessage(
    uint64_t stmId, uint64_t seqNum, bool endOfStm = false)
{
    auto serverMsg = std::make_unique<clproto::ServerMessage>();
    auto stmMsg = serverMsg->mutable_message_stream();
    stmMsg->set_stream_id(stmId);
    stmMsg->set_sequence_number(seqNum);

    if (endOfStm)
        serverMsg->mutable_end_of_stream();

    return std::move(serverMsg);
}

struct LowerLayer {
    using Callback = std::function<void(const std::error_code &)>;
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD0(connect, void());
    MOCK_METHOD1(
        setOnMessageCallback, void(std::function<void(ServerMessagePtr)>));
    MOCK_METHOD1(sendProxy, void(const clproto::ClientMessage &));

    void send(ClientMessagePtr cmp, Callback) { sendProxy(*cmp); }
};

struct SequencerTest : public ::testing::Test {
    SequencerTest()
    {
        scheduler = std::make_shared<MockScheduler>();
        EXPECT_CALL(*scheduler, schedule(_, _)).Times(AnyNumber());
        EXPECT_CALL(sequencer.mock, sendProxy(_)).Times(AnyNumber());
    }

    std::shared_ptr<MockScheduler> scheduler;
    layers::Sequencer<LowerLayer, MockScheduler> sequencer;
};

TEST_F(SequencerTest, sequencerShouldPassNonStreamMessages)
{
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_)).Times(0);

    bool called = false;

    auto handleOnMessageCallback =
        [&](ServerMessagePtr) mutable { called = true; };
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    sequenceOnMessageCallback(std::make_unique<clproto::ServerMessage>());
    EXPECT_TRUE(called);
}

TEST_F(SequencerTest, sequencerShouldPassMessagesInOrder)
{
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_)).Times(AnyNumber());

    int called = 0;

    auto handleOnMessageCallback = [&](ServerMessagePtr) mutable { ++called; };
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    for (int stmId = 1; stmId <= 10; ++stmId) {
        for (int seqNum = 9; seqNum > 0; --seqNum) {
            sequenceOnMessageCallback(streamMessage(stmId, seqNum));
            EXPECT_EQ((stmId - 1) * 10, called);
        }

        sequenceOnMessageCallback(streamMessage(stmId, 0));
        EXPECT_EQ(stmId * 10, called);
    }
}

TEST_F(SequencerTest, sequencerShouldNotForwardPastDuplicatedMessages)
{
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_)).Times(AnyNumber());

    int called = 0;

    auto handleOnMessageCallback = [&](ServerMessagePtr) mutable { ++called; };
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    for (int i = 0; i < 2; ++i) {
        sequenceOnMessageCallback(streamMessage(1, 0));
        EXPECT_EQ(1, called);
    }
}

TEST_F(SequencerTest, sequencerShouldNotForwardFutureDuplicatedMessages)
{
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_)).Times(AnyNumber());

    int called = 0;

    auto handleOnMessageCallback = [&](ServerMessagePtr) mutable { ++called; };
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    for (int i = 0; i < 2; ++i) {
        sequenceOnMessageCallback(streamMessage(1, 1));
        EXPECT_EQ(0, called);
    }

    sequenceOnMessageCallback(streamMessage(1, 0));
    EXPECT_EQ(2, called);
}

TEST_F(SequencerTest, sequencerShouldRemoveBufferWhenEndOfStreamMessageReceived)
{
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_)).Times(AnyNumber());

    int called = 0;

    auto handleOnMessageCallback = [&](ServerMessagePtr) mutable { ++called; };
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    sequenceOnMessageCallback(streamMessage(1, 1, true));
    EXPECT_EQ(0, called);
    sequenceOnMessageCallback(streamMessage(1, 0));
    EXPECT_EQ(2, called);
    sequenceOnMessageCallback(streamMessage(1, 0));
    EXPECT_EQ(3, called);
}

TEST_F(SequencerTest,
    sequencerShouldSchedulePeriodicMessageRequestOnInitialization)
{
    EXPECT_CALL(*scheduler, schedule(_, _)).Times(1);
    EXPECT_CALL(sequencer.mock, connect());
    sequencer.setScheduler(scheduler);
    sequencer.connect();
}

TEST_F(SequencerTest, sequencerShouldSendMessageStreamResetOnInitialization)
{
    EXPECT_CALL(sequencer.mock, sendProxy(_)).Times(1);
    EXPECT_CALL(sequencer.mock, connect());
    sequencer.setScheduler(scheduler);
    sequencer.connect();
}

TEST_F(SequencerTest, sequencerShouldSendMessageRequestMessage)
{
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_))
        .Times(1)
        .WillOnce(Invoke([](const clproto::ClientMessage &msg) {
            EXPECT_TRUE(msg.has_message_request());
            EXPECT_EQ(1, msg.message_request().stream_id());
            EXPECT_EQ(0, msg.message_request().lower_sequence_number());
            EXPECT_EQ(9, msg.message_request().upper_sequence_number());
        }));

    bool called = false;

    auto handleOnMessageCallback =
        [&](ServerMessagePtr) mutable { called = true; };
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    sequenceOnMessageCallback(streamMessage(1, 10));
    EXPECT_FALSE(called);
}

TEST_F(SequencerTest, sequencerShouldSendMultipleMessageRequestMessages)
{
    int msgReqCtr = 0;
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_))
        .Times(AtLeast(1000))
        .WillRepeatedly(Invoke([&](const clproto::ClientMessage &msg) {
            if (msg.has_message_request())
                ++msgReqCtr;
        }));

    auto handleOnMessageCallback = [&](ServerMessagePtr) mutable {};
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    for (int i = 1000; i > 0; --i)
        sequenceOnMessageCallback(streamMessage(1, i));

    EXPECT_EQ(1000, msgReqCtr);
}

TEST_F(SequencerTest, sequencerShouldSendMessageAcknowledgementMessage)
{
    int msgAckCtr = 0;
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_))
        .WillRepeatedly(Invoke([&](const clproto::ClientMessage &msg) {
            if (msg.has_message_acknowledgement()) {
                ++msgAckCtr;
                EXPECT_EQ(1, msg.message_acknowledgement().stream_id());
                EXPECT_EQ(99, msg.message_acknowledgement().sequence_number());
            }
        }));

    auto handleOnMessageCallback = [&](ServerMessagePtr) mutable {};
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    std::vector<int> n(STREAM_MSG_ACK_WINDOW);
    std::iota(n.begin(), n.end(), 0);
    std::random_shuffle(n.begin(), n.end());

    for (auto i : n)
        sequenceOnMessageCallback(streamMessage(1, i));

    EXPECT_EQ(1, msgAckCtr);
}

TEST_F(SequencerTest, sequencerShouldSendMultipleMessageAcknowledgementMessages)
{
    int msgAckCtr = 0;
    int lastSeqNum = 0;
    std::function<void(ServerMessagePtr)> sequenceOnMessageCallback;
    EXPECT_CALL(sequencer.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&sequenceOnMessageCallback));
    EXPECT_CALL(sequencer.mock, sendProxy(_))
        .Times(AtLeast(1))
        .WillRepeatedly(Invoke([&](const clproto::ClientMessage &msg) {
            if (msg.has_message_acknowledgement()) {
                ++msgAckCtr;
                lastSeqNum = msg.message_acknowledgement().sequence_number();
            }
        }));

    auto handleOnMessageCallback = [&](ServerMessagePtr) mutable {};
    sequencer.setOnMessageCallback(handleOnMessageCallback);

    std::vector<int> n(10 * STREAM_MSG_ACK_WINDOW);
    std::iota(n.begin(), n.end(), 0);
    std::random_shuffle(n.begin(), n.end());

    for (auto i : n)
        sequenceOnMessageCallback(streamMessage(1, i));

    EXPECT_GE(1, msgAckCtr);
    EXPECT_EQ(10 * STREAM_MSG_ACK_WINDOW - 1, lastSeqNum);
}
