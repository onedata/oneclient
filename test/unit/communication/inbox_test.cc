/**
 * @file inbox_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/declarations.h"
#include "communication/exception.h"
#include "communication/layers/inbox.h"
#include "testUtils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <functional>

using namespace one;
using namespace one::communication;
using namespace one::testing;
using namespace ::testing;

struct LowerLayer {
    using Callback = std::function<void(const std::error_code &)>;
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD2(sendProxy, void(clproto::ClientMessage, int));
    MOCK_METHOD0(connect, void());
    MOCK_METHOD1(
        setOnMessageCallback, void(std::function<void(ServerMessagePtr)>));

    void send(ClientMessagePtr cmp, Callback callback, int i)
    {
        if (error) {
            callback(std::make_error_code(std::errc::bad_message));
        }
        else {
            sendProxy(*cmp, i);
            callback(std::error_code{});
        }
    }

    bool error = false;
};

struct InboxTest : public ::testing::Test {
    layers::Inbox<LowerLayer> inbox;
};

ACTION_P(SaveMessageId, result)
{
    ASSERT_TRUE(arg0.has_message_id());
    *result = arg0.message_id();
}

TEST_F(InboxTest, communicateShouldPassArgumentsToSend)
{
    const auto data = randomString();
    const auto retries = randomInt();

    clproto::ClientMessage sentMsg;
    EXPECT_CALL(inbox.mock, sendProxy(_, retries))
        .WillOnce(SaveArg<0>(&sentMsg));

    auto msg = std::make_unique<clproto::ClientMessage>();
    auto ping = msg->mutable_ping();
    ping->set_data(data);

    inbox.communicate(std::move(msg), [](auto, auto) {}, retries);

    ASSERT_EQ(data, sentMsg.ping().data());
}

TEST_F(InboxTest, communicateShouldSetMessageId)
{
    std::string sentMsgId1;
    std::string sentMsgId2;
    EXPECT_CALL(inbox.mock, sendProxy(_, _))
        .WillOnce(SaveMessageId(&sentMsgId1))
        .WillOnce(SaveMessageId(&sentMsgId2));

    auto msg = std::make_unique<clproto::ClientMessage>();
    inbox.communicate(std::move(msg), [](auto, auto) {}, randomInt());

    EXPECT_FALSE(sentMsgId1.empty());

    msg = std::make_unique<clproto::ClientMessage>();
    inbox.communicate(std::move(msg), [](auto, auto) {}, randomInt());

    EXPECT_FALSE(sentMsgId2.empty());
    EXPECT_NE(sentMsgId1, sentMsgId2);
}

TEST_F(InboxTest, communicateShouldReturnExceptionIfSendFails)
{
    inbox.mock.error = true;

    auto msg = std::make_unique<clproto::ClientMessage>();
    inbox.communicate(
        std::move(msg), [](auto ec, auto) { ASSERT_TRUE(!!ec); }, randomInt());
}

TEST_F(InboxTest, InboxShouldFulfillCommunicatePromiseOnceReplyIsReceived)
{
    std::string sentMsgId;
    EXPECT_CALL(inbox.mock, sendProxy(_, _))
        .WillOnce(SaveMessageId(&sentMsgId));

    std::function<void(ServerMessagePtr)> onMessageCallback;
    EXPECT_CALL(inbox.mock, connect());
    EXPECT_CALL(inbox.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&onMessageCallback));

    inbox.connect();

    clproto::ServerMessage result;
    auto msg = std::make_unique<clproto::ClientMessage>();
    inbox.communicate(
        std::move(msg), [&](auto, auto r) { result = *r; }, randomInt());

    ASSERT_FALSE(result.has_pong());

    const auto data = randomString();

    auto replyMsg = std::make_unique<clproto::ServerMessage>();
    replyMsg->set_message_id(sentMsgId);
    auto pong = replyMsg->mutable_pong();
    pong->set_data(data);

    onMessageCallback(std::move(replyMsg));
    ASSERT_TRUE(result.has_pong());
    ASSERT_EQ(data, result.pong().data());
}

TEST_F(InboxTest, connectShouldSetOnMessageCallback)
{
    EXPECT_CALL(inbox.mock, connect());
    EXPECT_CALL(inbox.mock, setOnMessageCallback(_));

    inbox.connect();
}

TEST_F(InboxTest, InboxShouldPassIncomingMessagesToSubscribers)
{
    std::function<void(ServerMessagePtr)> onMessageCallback;
    EXPECT_CALL(inbox.mock, connect());
    EXPECT_CALL(inbox.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&onMessageCallback));

    bool predicateCalled = false;
    bool callbackCalled = false;
    const auto messageId = randomString();

    inbox.connect();

    auto predicate = [&](auto, bool) mutable {
        predicateCalled = true;
        return true;
    };
    auto callback = [&](const clproto::ServerMessage &msg) mutable {
        callbackCalled = true;
        ASSERT_EQ(messageId, msg.message_id());
    };

    inbox.subscribe({predicate, callback});

    auto msg = std::make_unique<clproto::ServerMessage>();
    msg->set_message_id(messageId);
    onMessageCallback(std::move(msg));

    ASSERT_TRUE(predicateCalled);
    ASSERT_TRUE(callbackCalled);
}

TEST_F(InboxTest, InboxShouldMarkExpectedMessagesAsHandledForSubscribers)
{
    std::function<void(ServerMessagePtr)> onMessageCallback;
    EXPECT_CALL(inbox.mock, connect());
    EXPECT_CALL(inbox.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&onMessageCallback));

    const auto messageId = randomString();

    inbox.connect();
    int predicateCalled = 0;

    auto predicate = [&](const clproto::ServerMessage &msg, bool handled) {
        ++predicateCalled;

        if (msg.message_id() == messageId)
            EXPECT_FALSE(handled);
        else
            EXPECT_TRUE(handled);

        return false;
    };

    inbox.subscribe({predicate, [](auto) {}});

    auto msg = std::make_unique<clproto::ServerMessage>();
    msg->set_message_id(messageId);
    onMessageCallback(std::move(msg));

    ASSERT_EQ(1, predicateCalled);

    std::string sentMsgId;
    EXPECT_CALL(inbox.mock, sendProxy(_, _))
        .WillOnce(SaveMessageId(&sentMsgId));

    clproto::ServerMessage result;
    auto clientMsg = std::make_unique<clproto::ClientMessage>();

    inbox.communicate(
        std::move(clientMsg), [&](auto, auto r) { result = *r; }, randomInt());

    msg = std::make_unique<clproto::ServerMessage>();
    msg->set_message_id(sentMsgId);
    onMessageCallback(std::move(msg));

    ASSERT_EQ(2, predicateCalled);
    ASSERT_EQ(sentMsgId, result.message_id());
}

TEST_F(InboxTest, InboxShouldNotCallSubscriptionCallbackWhenPredicateIsFalse)
{
    std::function<void(ServerMessagePtr)> onMessageCallback;
    EXPECT_CALL(inbox.mock, connect());
    EXPECT_CALL(inbox.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&onMessageCallback));

    bool predicateCalled = false;
    inbox.connect();

    auto predicate = [&](auto, bool) mutable {
        predicateCalled = true;
        return false;
    };

    inbox.subscribe({predicate, [](auto) { FAIL(); }});

    auto msg = std::make_unique<clproto::ServerMessage>();
    msg->set_message_id(randomString());
    onMessageCallback(std::move(msg));

    ASSERT_TRUE(predicateCalled);
}
