/**
 * @file translator_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/declarations.h"
#include "communication/layers/translator.h"
#include "messages/ping.h"
#include "messages/pong.h"
#include "messages/handshakeRequest.h"
#include "messages/handshakeResponse.h"
#include "testUtils.h"

#include <boost/thread/future.hpp>
#include <boost/thread/executors/basic_thread_pool.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <chrono>
#include <functional>
#include <memory>
#include <string>

using namespace one;
using namespace one::communication;
using namespace ::testing;
using namespace std::literals;

struct LowerLayer {
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD2(sendProxy, void(clproto::ClientMessage, int));

    MOCK_METHOD2(communicateProxy, std::string(clproto::ClientMessage, int));

    MOCK_METHOD2(setHandshake, void(std::function<ClientMessagePtr()>,
                                   std::function<bool(ServerMessagePtr)>));

    MOCK_METHOD3(replyProxy,
        void(const clproto::ServerMessage, const clproto::ClientMessage, int));

    boost::future<void> send(ClientMessagePtr cmp, int i)
    {
        sendProxy(*cmp, i);
        return {};
    }

    auto communicate(ClientMessagePtr cmp, int i)
    {
        std::string data = communicateProxy(*cmp, i);

        auto msg = std::make_unique<clproto::ServerMessage>();
        auto pong = msg->mutable_pong();
        pong->set_data(data);

        return boost::make_ready_future(std::move(msg));
    }

    auto reply(const clproto::ServerMessage &a, ClientMessagePtr cmp, int i)
    {
        replyProxy(a, *cmp, i);
        return boost::make_ready_future();
    }

    boost::basic_thread_pool m_ioServiceExecutor{1};
};

struct TranslatorTest : public ::testing::Test {
    layers::Translator<LowerLayer> translator;
};

TEST_F(TranslatorTest, sendShouldSerializeDomainObjects)
{
    clproto::ClientMessage sentMsg;
    EXPECT_CALL(translator.mock, sendProxy(_, 1))
        .WillOnce(SaveArg<0>(&sentMsg));

    const auto data = randomString();
    translator.send(messages::Ping{data}, 1);

    ASSERT_TRUE(sentMsg.has_ping());
    ASSERT_TRUE(sentMsg.ping().has_data());
    ASSERT_EQ(data, sentMsg.ping().data());
}

TEST_F(TranslatorTest, sendShouldPassUninterestingArgumentsDown)
{
    const auto retries = randomInt();
    EXPECT_CALL(translator.mock, sendProxy(_, retries)).Times(1);
    translator.send(messages::Ping{}, retries);
}

TEST_F(TranslatorTest, communicateShouldSerializeDomainObjects)
{
    clproto::ClientMessage sentMsg;
    EXPECT_CALL(translator.mock, communicateProxy(_, 1))
        .WillOnce(DoAll(SaveArg<0>(&sentMsg), Return(std::string{})));

    const auto data = randomString();
    translator.communicate<messages::Pong>(messages::Ping{data}, 1);

    ASSERT_TRUE(sentMsg.has_ping());
    ASSERT_TRUE(sentMsg.ping().has_data());
    ASSERT_EQ(data, sentMsg.ping().data());
}

TEST_F(TranslatorTest, communicateShouldDeserializeProtocolObjects)
{
    const auto data = randomString();

    EXPECT_CALL(translator.mock, communicateProxy(_, 1)).WillOnce(Return(data));

    auto pong =
        translator.communicate<messages::Pong>(messages::Ping{data}, 1).get(1s);

    ASSERT_EQ(data, pong.data());
}

TEST_F(TranslatorTest, communicateShouldPassUninterestingArgumentsDown)
{
    const auto retries = randomInt();
    EXPECT_CALL(translator.mock, communicateProxy(_, retries)).Times(1);
    translator.communicate<messages::Pong>(messages::Ping{}, retries);
}

TEST_F(TranslatorTest, replyShouldSerializeDomainObjects)
{
    clproto::ClientMessage sentMsg;
    EXPECT_CALL(translator.mock, replyProxy(_, _, 1))
        .WillOnce(SaveArg<1>(&sentMsg));

    clproto::ServerMessage replyTo;

    const auto data = randomString();
    translator.reply(replyTo, messages::Ping{data}, 1);

    ASSERT_TRUE(sentMsg.has_ping());
    ASSERT_TRUE(sentMsg.ping().has_data());
    ASSERT_EQ(data, sentMsg.ping().data());
}

TEST_F(TranslatorTest, replyShouldPassUninterestingArgumentsDown)
{
    const auto retries = randomInt();
    const auto data = randomString();

    clproto::ServerMessage replyTo;
    auto pong = replyTo.mutable_pong();
    pong->set_data(data);

    clproto::ServerMessage passedArg;

    EXPECT_CALL(translator.mock, replyProxy(_, _, retries))
        .WillOnce(SaveArg<0>(&passedArg));

    translator.reply(replyTo, messages::Ping{}, retries);

    ASSERT_EQ(data, passedArg.pong().data());
}

TEST_F(TranslatorTest, setHandshakeShouldSerializeDomainObjects)
{
    std::function<ClientMessagePtr()> protoHandshakeF;

    EXPECT_CALL(translator.mock, setHandshake(_, _))
        .WillOnce(SaveArg<0>(&protoHandshakeF));

    const auto data = randomString();

    auto domainHandshakeF = [&]() { return messages::HandshakeRequest{data}; };
    translator.setHandshake(domainHandshakeF, [&](auto) { return true; });

    auto protoHandshake = protoHandshakeF();
    ASSERT_TRUE(protoHandshake->has_handshake_request());
    ASSERT_EQ(data, protoHandshake->handshake_request().session_id());
}

TEST_F(TranslatorTest, onHandshakeResponseShouldDeserializeProtocolObjects)
{
    std::function<bool(ServerMessagePtr)> protoHandshakeResponseF;

    EXPECT_CALL(translator.mock, setHandshake(_, _))
        .WillOnce(SaveArg<1>(&protoHandshakeResponseF));

    const auto data = randomString();
    bool called = false;

    auto domainHandshakeF = [&]() { return messages::HandshakeRequest{data}; };
    auto domainHandshakeResponseF =
        [&](one::messages::HandshakeResponse msg) mutable {
            called = true;
            EXPECT_EQ(data, msg.sessionId());
            return true;
        };

    translator.setHandshake(domainHandshakeF, domainHandshakeResponseF);

    auto msg = std::make_unique<clproto::ServerMessage>();
    auto response = msg->mutable_handshake_response();
    response->set_session_id(data);

    protoHandshakeResponseF(std::move(msg));
    ASSERT_TRUE(called);
}
