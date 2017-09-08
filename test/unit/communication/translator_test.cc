/**
 * @file translator_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/declarations.h"
#include "communication/layers/translator.h"
#include "errors/handshakeErrors.h"
#include "messages/handshakeRequest.h"
#include "messages/handshakeResponse.h"
#include "messages/ping.h"
#include "messages/pong.h"
#include "testUtils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <chrono>
#include <functional>
#include <memory>
#include <string>

using namespace one;
using namespace one::communication;
using namespace one::testing;
using namespace one::errors::handshake;
using namespace ::testing;
using namespace std::literals;

struct LowerLayer {
    using Callback = std::function<void(const std::error_code &)>;
    using CommunicateCallback =
        std::function<void(const std::error_code &ec, ServerMessagePtr)>;

    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD2(sendProxy, void(clproto::ClientMessage, int));

    MOCK_METHOD2(communicateProxy, std::string(clproto::ClientMessage, int));

    MOCK_METHOD3(setHandshake,
        void(std::function<ClientMessagePtr()>,
            std::function<std::error_code(ServerMessagePtr)>,
            std::function<void(std::error_code)>));

    MOCK_METHOD3(replyProxy,
        void(const clproto::ServerMessage, const clproto::ClientMessage, int));

    void send(ClientMessagePtr cmp, Callback, int i) { sendProxy(*cmp, i); }

    void communicate(ClientMessagePtr cmp, CommunicateCallback callback, int i)
    {
        std::string data = communicateProxy(*cmp, i);

        auto msg = std::make_unique<clproto::ServerMessage>();
        auto pong = msg->mutable_pong();
        pong->set_data(data);

        callback(std::error_code{}, std::move(msg));
    }

    auto reply(const clproto::ServerMessage &a, ClientMessagePtr cmp,
        Callback callback, int i)
    {
        replyProxy(a, *cmp, i);
        callback(std::error_code{});
    }
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
        translator.communicate<messages::Pong>(messages::Ping{data}, 1).get();

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

    EXPECT_CALL(translator.mock, setHandshake(_, _, _))
        .WillOnce(SaveArg<0>(&protoHandshakeF));

    const auto data = randomString();

    auto domainHandshakeF = [&]() { return messages::HandshakeRequest{data}; };
    translator.setHandshake(
        domainHandshakeF, [&](auto) { return std::error_code{}; });

    auto protoHandshake = protoHandshakeF();
    ASSERT_TRUE(protoHandshake->has_handshake_request());
    ASSERT_EQ(data, protoHandshake->handshake_request().session_id());
}

TEST_F(TranslatorTest, onHandshakeResponseShouldDeserializeProtocolObjects)
{
    std::function<std::error_code(ServerMessagePtr)> protoHandshakeResponseF;

    EXPECT_CALL(translator.mock, setHandshake(_, _, _))
        .WillOnce(SaveArg<1>(&protoHandshakeResponseF));

    const auto sessionId = randomString();
    bool called = false;

    auto domainHandshakeF = [&]() {
        return messages::HandshakeRequest{sessionId};
    };
    auto domainHandshakeResponseF =
        [&](one::messages::HandshakeResponse msg) mutable {
            called = true;
            EXPECT_EQ(makeErrorCode(ErrorCode::ok), msg.status());
            return msg.status();
        };

    translator.setHandshake(domainHandshakeF, domainHandshakeResponseF);

    auto msg = std::make_unique<clproto::ServerMessage>();
    auto response = msg->mutable_handshake_response();
    response->set_status(clproto::HandshakeStatus::OK);

    protoHandshakeResponseF(std::move(msg));
    ASSERT_TRUE(called);
}
