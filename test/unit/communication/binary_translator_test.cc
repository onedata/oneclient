/**
 * @file binary_translator_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "communication/layers/binaryTranslator.h"
#include "messages/ping.h"
#include "messages/pong.h"
#include "testUtils.h"

#include <boost/thread/future.hpp>
#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <string>

using namespace one;
using namespace one::communication;
using namespace std::literals;
using namespace ::testing;

struct LowerLayer {
    LowerLayer &mock = static_cast<LowerLayer &>(*this);

    MOCK_METHOD2(sendProxy, void(std::string, int));
    MOCK_METHOD1(setOnMessageCallback, void(std::function<void(std::string)>));
    MOCK_METHOD2(setHandshake,
        void(std::function<std::string()>, std::function<bool(std::string)>));

    auto send(std::string msg, int i)
    {
        sendProxy(std::move(msg), i);
        return boost::make_ready_future();
    }
};

struct BinaryTranslatorTest : public ::testing::Test {
    layers::BinaryTranslator<LowerLayer> binaryTranslator;
};

TEST_F(BinaryTranslatorTest, sendShouldSerializeProtocolObjects)
{
    const auto data = randomString();

    messages::Ping ping{data};
    auto protoMsg = ping.serialize();
    auto msg = protoMsg->SerializeAsString();

    EXPECT_CALL(binaryTranslator.mock, sendProxy(msg, _));

    binaryTranslator.send(std::move(protoMsg), randomInt());
}

TEST_F(BinaryTranslatorTest, sendShouldPassUninterestingArgumentsDown)
{
    const auto retries = randomInt();
    EXPECT_CALL(binaryTranslator.mock, sendProxy(_, retries));
    binaryTranslator.send(messages::Ping{randomString()}.serialize(), retries);
}

TEST_F(BinaryTranslatorTest, setOnMessageCallbackShouldDeserializeBytes)
{
    std::function<void(std::string)> byteOnMessageCallback;
    EXPECT_CALL(binaryTranslator.mock, setOnMessageCallback(_))
        .WillOnce(SaveArg<0>(&byteOnMessageCallback));

    const auto data = randomString();
    bool called = false;

    auto protoOnMessageCallback = [&](ServerMessagePtr msg) mutable {
        called = true;
        ASSERT_EQ(data, msg->message_id());
    };

    binaryTranslator.setOnMessageCallback(protoOnMessageCallback);

    clproto::ServerMessage protoMsg;
    protoMsg.set_message_id(data);
    byteOnMessageCallback(protoMsg.SerializeAsString());

    ASSERT_TRUE(called);
}

TEST_F(BinaryTranslatorTest, setHandshakeShouldSerializeDomainObjects)
{
    std::function<std::string()> byteGetHandshake;
    EXPECT_CALL(binaryTranslator.mock, setHandshake(_, _))
        .WillOnce(SaveArg<0>(&byteGetHandshake));

    const auto data = randomString();
    auto protoMsg = messages::Ping{data}.serialize();
    auto msg = protoMsg->SerializeAsString();

    auto protoGetHandshake = [&] { return std::move(protoMsg); };
    binaryTranslator.setHandshake(protoGetHandshake, [](auto) { return true; });

    ASSERT_EQ(msg, byteGetHandshake());
}

TEST_F(BinaryTranslatorTest, setHandshakeShouldDeserializeBytes)
{
    std::function<bool(std::string)> byteOnHandshakeResponse;
    EXPECT_CALL(binaryTranslator.mock, setHandshake(_, _))
        .WillOnce(SaveArg<1>(&byteOnHandshakeResponse));

    const auto data = randomString();
    auto protoMsg = std::make_unique<clproto::ServerMessage>();
    protoMsg->set_message_id(data);

    bool called = false;

    auto protoOnHandshakeResponse = [&](ServerMessagePtr msg) mutable {
        called = true;
        EXPECT_EQ(data, msg->message_id());
        return true;
    };

    binaryTranslator.setHandshake(
        [] { return ClientMessagePtr{}; }, protoOnHandshakeResponse);

    byteOnHandshakeResponse(protoMsg->SerializeAsString());

    ASSERT_TRUE(called);
}
