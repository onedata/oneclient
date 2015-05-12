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

#include <boost/thread/future.hpp>
#include <boost/thread/executors/basic_thread_pool.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <functional>

using namespace one;
using namespace one::communication;
using namespace ::testing;

struct LowerLayer {
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD2(sendProxy, void(clproto::ClientMessage, int));
    MOCK_METHOD0(connect, void());
    MOCK_METHOD1(
        setOnMessageCallback, void(std::function<void(ServerMessagePtr)>));

    boost::future<void> send(ClientMessagePtr cmp, int i)
    {
        if (exception)
            return boost::make_exceptional(ConnectionError("error"));

        sendProxy(*cmp, i);
        return boost::make_ready_future();
    }

    boost::basic_thread_pool m_ioServiceExecutor{1};
    bool exception = false;
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

    inbox.communicate(std::move(msg), retries);

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
    inbox.communicate(std::move(msg), randomInt());

    EXPECT_FALSE(sentMsgId1.empty());

    msg = std::make_unique<clproto::ClientMessage>();
    inbox.communicate(std::move(msg), randomInt());

    EXPECT_FALSE(sentMsgId2.empty());
    EXPECT_NE(sentMsgId1, sentMsgId2);
}

TEST_F(InboxTest, communicateShouldReturnExceptionIfSendFails)
{
    inbox.mock.exception = true;

    auto msg = std::make_unique<clproto::ClientMessage>();
    auto future = inbox.communicate(std::move(msg), randomInt());

    ASSERT_THROW(future.get(), ConnectionError);
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

    auto msg = std::make_unique<clproto::ClientMessage>();
    auto future = inbox.communicate(std::move(msg), randomInt());

    ASSERT_EQ(future.get_state(), boost::future_state::waiting);

    const auto data = randomString();

    auto replyMsg = std::make_unique<clproto::ServerMessage>();
    replyMsg->set_message_id(sentMsgId);
    auto pong = replyMsg->mutable_pong();
    pong->set_data(data);

    onMessageCallback(std::move(replyMsg));
    ASSERT_EQ(data, future.get()->pong().data());
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

    auto clientMsg = std::make_unique<clproto::ClientMessage>();
    auto f = inbox.communicate(std::move(clientMsg), randomInt());

    msg = std::make_unique<clproto::ServerMessage>();
    msg->set_message_id(sentMsgId);
    onMessageCallback(std::move(msg));

    ASSERT_EQ(2, predicateCalled);
    ASSERT_EQ(sentMsgId, f.get()->message_id());
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
