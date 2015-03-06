/**
 * @file communicationHandler_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication/communicationHandler.h"

#include "communication/connection.h"
#include "communication/connectionPool.h"
#include "communication/connectionPool_mock.h"
#include "testUtils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <chrono>
#include <functional>
#include <memory>
#include <random>
#include <string>
#include <thread>

using namespace ::testing;
using namespace std::placeholders;

struct CommunicationHandlerTest: public ::testing::Test
{
    ConnectionPoolMock *dataPool;
    ConnectionPoolMock *metaPool;
    std::unique_ptr<one::communication::CommunicationHandler> communicationHandler;

    using Pool = one::communication::CommunicationHandler::Pool;

    CommunicationHandlerTest()
    {
        auto dataPoolPtr = std::make_unique<NiceMock<ConnectionPoolMock>>();
        auto metaPoolPtr = std::make_unique<NiceMock<ConnectionPoolMock>>();

        dataPool = dataPoolPtr.get();
        metaPool = metaPoolPtr.get();

        communicationHandler = std::make_unique<one::communication::CommunicationHandler>(
                    std::move(dataPoolPtr), std::move(metaPoolPtr));
    }
};

one::clproto::communication_protocol::ClusterMsg randomMessage()
{
    one::clproto::communication_protocol::ClusterMsg message;
    message.set_answer_decoder_name(randomString());
    message.set_answer_type(randomString());
    message.set_input(randomString());
    message.set_message_decoder_name(randomString());
    message.set_message_type(randomString());
    message.set_module_name(randomString());
    message.set_protocol_version(randomInt());
    message.set_synch(true);

    return message;
}

std::unique_ptr<one::clproto::communication_protocol::ClusterMsg> randomHandshake()
{
    return std::make_unique<one::clproto::communication_protocol::ClusterMsg>(randomMessage());
};

CommunicationHandlerTest::Pool randomPool()
{
    return std::bernoulli_distribution{0.5}(gen)
        ? CommunicationHandlerTest::Pool::META
        : CommunicationHandlerTest::Pool::DATA;
}

TEST_F(CommunicationHandlerTest, shouldSetOnMessageCallbackOnPools)
{
    auto dataPool = std::make_unique<NiceMock<ConnectionPoolMock>>();
    auto metaPool = std::make_unique<NiceMock<ConnectionPoolMock>>();

    EXPECT_CALL(*dataPool, setOnMessageCallback(_));
    EXPECT_CALL(*metaPool, setOnMessageCallback(_));

    std::make_unique<one::communication::CommunicationHandler>(
                std::move(dataPool), std::move(metaPool));
}

TEST_F(CommunicationHandlerTest, shouldCallSendOnAppropriatePoolOnReply)
{
    auto dataMsg = randomMessage();
    auto metaMsg = randomMessage();

    std::string sentDataMessage;
    std::string sentMetaMessage;

    EXPECT_CALL(*dataPool, send(_)).WillOnce(SaveArg<0>(&sentDataMessage));
    EXPECT_CALL(*metaPool, send(_)).WillOnce(SaveArg<0>(&sentMetaMessage));

    one::clproto::communication_protocol::Answer replyTo;
    replyTo.set_message_id(0);

    communicationHandler->reply(replyTo, dataMsg, Pool::DATA, 0);
    communicationHandler->reply(replyTo, metaMsg, Pool::META, 0);

    ASSERT_EQ(dataMsg.SerializeAsString(), sentDataMessage);
    ASSERT_EQ(metaMsg.SerializeAsString(), sentMetaMessage);
}


TEST_F(CommunicationHandlerTest, shouldCallSendOnAppropriatePoolOnSend)
{
    auto dataMsg = randomMessage();
    auto metaMsg = randomMessage();

    std::string sentDataMessage;
    std::string sentMetaMessage;

    EXPECT_CALL(*dataPool, send(_)).WillOnce(SaveArg<0>(&sentDataMessage));
    EXPECT_CALL(*metaPool, send(_)).WillOnce(SaveArg<0>(&sentMetaMessage));

    communicationHandler->send(dataMsg, Pool::DATA, 0);
    communicationHandler->send(metaMsg, Pool::META, 0);

    ASSERT_EQ(dataMsg.SerializeAsString(), sentDataMessage);
    ASSERT_EQ(metaMsg.SerializeAsString(), sentMetaMessage);
}

TEST_F(CommunicationHandlerTest, shouldSetMessageIdOnSend)
{
    auto dataMsg = randomMessage();

    ASSERT_FALSE(dataMsg.has_message_id());
    communicationHandler->send(dataMsg, randomPool(), 0);
    ASSERT_TRUE(dataMsg.has_message_id());
}

TEST_F(CommunicationHandlerTest, shouldSetIncrementingMessageIds)
{
    auto dataMsg = randomMessage();

    communicationHandler->send(dataMsg, randomPool(), 0);
    auto msgId = dataMsg.message_id();

    communicationHandler->send(dataMsg, randomPool(), 0);
    ASSERT_GT(dataMsg.message_id(), msgId);
}

TEST_F(CommunicationHandlerTest, shouldCallSendOnAppropriatePoolOnCommunicate)
{
    auto dataMsg = randomMessage();
    auto metaMsg = randomMessage();

    std::string sentDataMessage;
    std::string sentMetaMessage;

    EXPECT_CALL(*dataPool, send(_)).WillOnce(SaveArg<0>(&sentDataMessage));
    EXPECT_CALL(*metaPool, send(_)).WillOnce(SaveArg<0>(&sentMetaMessage));

    communicationHandler->communicate(dataMsg, Pool::DATA, 0);
    communicationHandler->communicate(metaMsg, Pool::META, 0);

    ASSERT_EQ(dataMsg.SerializeAsString(), sentDataMessage);
    ASSERT_EQ(metaMsg.SerializeAsString(), sentMetaMessage);
}

TEST_F(CommunicationHandlerTest, shouldFulfilAPromiseOnResultMessage)
{
    auto poolType = randomPool();
    auto pool = poolType == Pool::META ? metaPool : dataPool;
    auto dataMsg = randomMessage();

    auto future = communicationHandler->communicate(dataMsg, randomPool(), 0);
    auto messageId = dataMsg.message_id();

    one::clproto::communication_protocol::Answer answer;
    answer.set_answer_status("answer status");
    answer.set_message_id(messageId);

    std::thread t{[&]{ pool->onMessageCallback(answer.SerializeAsString()); }};

    auto status = future.wait_for(std::chrono::seconds{5});
    ASSERT_EQ(std::future_status::ready, status);
    EXPECT_EQ(answer.SerializeAsString(), future.get()->SerializeAsString());

    t.join();
}

TEST_F(CommunicationHandlerTest, shouldCallSubscribedCallbackOnPredicateFulfilment)
{
    auto pred = [](const one::clproto::communication_protocol::Answer &ans) {
        return ans.message_id() == 128 || ans.message_id() == 256 || ans.message_id() == 512;
    };

    int callbackCalled = 0;
    one::clproto::communication_protocol::Answer answerGiven;
    auto callback = [&](const one::clproto::communication_protocol::Answer &ans) {
        ++callbackCalled;
        answerGiven = ans;
    };

    communicationHandler->subscribe({pred, callback});

    for(int i = 0; i < 1000; ++i)
    {
        one::clproto::communication_protocol::Answer answer;
        answer.set_answer_status("answer status");
        answer.set_message_id(i);

        auto pool = randomPool() == CommunicationHandlerTest::Pool::META ? metaPool : dataPool;
        pool->onMessageCallback(answer.SerializeAsString());
    }

    EXPECT_EQ(3, callbackCalled);
    EXPECT_EQ(512, answerGiven.message_id());
}

TEST_F(CommunicationHandlerTest, shouldPassHandshakeAndGoodbyeToDataPool)
{
    EXPECT_CALL(*dataPool, addHandshake(_, _));
    communicationHandler->addHandshake(randomHandshake, randomHandshake, Pool::DATA);
}

TEST_F(CommunicationHandlerTest, shouldPassHandshakeAndGoodbyeToMetaPool)
{
    EXPECT_CALL(*metaPool, addHandshake(_, _));
    communicationHandler->addHandshake(randomHandshake, randomHandshake, Pool::META);
}

TEST_F(CommunicationHandlerTest, shouldPassHandshakeToDataPool)
{
    EXPECT_CALL(*dataPool, addHandshake(_));
    communicationHandler->addHandshake(randomHandshake, Pool::DATA);
}

TEST_F(CommunicationHandlerTest, shouldPassHandshakeToMetaPool)
{
    EXPECT_CALL(*metaPool, addHandshake(_));
    communicationHandler->addHandshake(randomHandshake, Pool::META);
}

void checkMessageGenerator(const std::function<std::string()> &gen,
                           one::clproto::communication_protocol::ClusterMsg original)
{
    ASSERT_FALSE(original.has_message_id());

    decltype(original) generated1;
    generated1.ParseFromString(gen());
    ASSERT_TRUE(generated1.has_message_id());

    decltype(original) generated2;
    generated2.ParseFromString(gen());
    ASSERT_TRUE(generated2.has_message_id());

    ASSERT_GE(generated2.message_id(), generated1.message_id());

    original.set_message_id(generated1.message_id());
    ASSERT_EQ(original.SerializeAsString(), generated1.SerializeAsString());

    original.set_message_id(generated2.message_id());
    ASSERT_EQ(original.SerializeAsString(), generated2.SerializeAsString());
}

TEST_F(CommunicationHandlerTest, shouldGenerateIdsForHandshakeAndGoodbyeMessages)
{
    using one::clproto::communication_protocol::ClusterMsg;

    auto poolType = randomPool();
    auto pool = poolType == Pool::META ? metaPool : dataPool;
    auto originalHandshakeMsg = randomMessage();
    auto originalGoodbyeMsg = randomMessage();

    auto originalHandshake = [&]{ return std::make_unique<ClusterMsg>(originalHandshakeMsg); };
    auto originalGoodbye = [&]{ return std::make_unique<ClusterMsg>(originalGoodbyeMsg); };

    std::function<std::string()> handshakeGenerator;
    std::function<std::string()> goodbyeGenerator;

    EXPECT_CALL(*pool, addHandshake(_, _)).WillOnce(
                DoAll(SaveArg<0>(&handshakeGenerator),
                      SaveArg<1>(&goodbyeGenerator),
                      Return([]{})));

    communicationHandler->addHandshake(originalHandshake, originalGoodbye, poolType);

    checkMessageGenerator(handshakeGenerator, originalHandshakeMsg);
    checkMessageGenerator(goodbyeGenerator, originalGoodbyeMsg);
}

TEST_F(CommunicationHandlerTest, shouldGenerateIdsForHandshakeMessages)
{
    using one::clproto::communication_protocol::ClusterMsg;

    auto poolType = randomPool();
    auto pool = poolType == Pool::META ? metaPool : dataPool;
    auto originalHandshakeMsg = randomMessage();
    auto originalHandshake = [&]{ return std::make_unique<ClusterMsg>(originalHandshakeMsg); };

    std::function<std::string()> handshakeGenerator;

    EXPECT_CALL(*pool, addHandshake(_)).WillOnce(
                DoAll(SaveArg<0>(&handshakeGenerator), Return([]{})));

    communicationHandler->addHandshake(originalHandshake, poolType);

    checkMessageGenerator(handshakeGenerator, originalHandshakeMsg);
}

TEST_F(CommunicationHandlerTest, shouldReturnHandshakeRemovalFunctionOnHandshakeAdd)
{
    const auto poolType = randomPool();
    auto pool = poolType == Pool::META ? metaPool : dataPool;

    const auto handshakeFun = [=]{
        return std::make_unique<one::clproto::communication_protocol::ClusterMsg>(randomMessage());
    };

    bool removeCalled = false;
    bool remove2Called = false;

    EXPECT_CALL(*pool, addHandshake(_)).WillOnce(Return([&]{ removeCalled = true; }));
    EXPECT_CALL(*pool, addHandshake(_, _)).WillOnce(Return([&]{ remove2Called = true; }));

    auto remove = communicationHandler->addHandshake(handshakeFun, poolType);
    auto remove2 = communicationHandler->addHandshake(handshakeFun, handshakeFun, poolType);

    remove();
    remove2();

    EXPECT_TRUE(removeCalled);
    EXPECT_TRUE(remove2Called);
}

TEST_F(CommunicationHandlerTest, shouldReplyWithProperMessageId)
{
    auto poolType = randomPool();
    auto msg = randomMessage();

    one::clproto::communication_protocol::Answer replyTo;
    for(auto attempts = randomInt(100, 1000); attempts >= 0; --attempts)
    {
        auto msgId = randomInt(-1000, 1000);
        replyTo.set_message_id(msgId);
        communicationHandler->reply(replyTo, msg, poolType, 0);

        ASSERT_EQ(msgId, msg.message_id());
    }
}
