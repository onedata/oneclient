/**
 * @file communicator_test.cc
 * @author Konrad Zemek
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication/communicator.h"

#include "communication_protocol.pb.h"
#include "communication/communicationHandler_mock.h"
#include "communication/connection.h"
#include "communication/exception.h"
#include "fuse_messages.pb.h"
#include "logging.pb.h"
#include "remote_file_management.pb.h"
#include "testUtils.h"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <functional>
#include <random>
#include <memory>
#include <mutex>
#include <numeric>
#include <string>
#include <thread>

using namespace ::testing;
using namespace std::placeholders;

struct CommunicatorTest: public ::testing::Test
{
    std::unique_ptr<one::communication::Communicator> communicator;
    MockCommunicationHandler *handlerMock;
    std::string fuseId;
    one::communication::ServerModule randomModule;

    CommunicatorTest()
    {
        fuseId = randomString();
        randomModule = one::communication::ServerModule::CLUSTER_RENGINE;

        auto p = std::make_unique<NiceMock<MockCommunicationHandler>>();
        handlerMock = p.get();

        communicator = std::make_unique<one::communication::Communicator>(
                    std::move(p));

        communicator->setFuseId(fuseId);
    }
};

TEST_F(CommunicatorTest, shouldAddHandshakeAndGoodbyeOnSetupPushChannels)
{
    std::function<std::unique_ptr<MockCommunicationHandler::Message>()> addedHandshake;
    std::function<std::unique_ptr<MockCommunicationHandler::Message>()> addedGoodbye;

    EXPECT_CALL(*handlerMock, addHandshake(_, _, one::communication::CommunicationHandler::Pool::META)).
            WillOnce(DoAll(SaveArg<0>(&addedHandshake), SaveArg<1>(&addedGoodbye),
                           Return([]{})));

    communicator->setupPushChannels({});

    one::clproto::fuse_messages::ChannelRegistration reg;
    one::clproto::fuse_messages::ChannelClose close;

    ASSERT_TRUE(reg.ParseFromString(addedHandshake()->input()));
    ASSERT_EQ(fuseId, reg.fuse_id());

    ASSERT_TRUE(close.ParseFromString(addedGoodbye()->input()));
    ASSERT_EQ(fuseId, close.fuse_id());
}

ACTION_P2(SaveFunctions, predicate, callback)
{
    *predicate = arg0.predicate;
    *callback = arg0.callback;
}

TEST_F(CommunicatorTest, shouldSubscribeForPushMessagesOnEnablePushChannel)
{
    std::function<bool(const MockCommunicationHandler::Answer&)> subscribedPredicate;
    std::function<void(const MockCommunicationHandler::Answer&)> subscribedCallback;

    EXPECT_CALL(*handlerMock, subscribe(_))
            .WillOnce(DoAll(SaveFunctions(&subscribedPredicate, &subscribedCallback),
                            Return([]{})));

    communicator->setupPushChannels([](const MockCommunicationHandler::Answer&){});

    std::bernoulli_distribution dis{0.5};
    MockCommunicationHandler::Answer answer;

    for(int i = randomInt(100, 1000); i >= 0; --i)
    {
        if(dis(gen))
        {
            answer.set_message_id(randomInt(std::numeric_limits<decltype(answer.message_id())>::min(), -1));
            ASSERT_TRUE(subscribedPredicate(answer));
            subscribedCallback(answer);
        }
        else
        {
            answer.set_message_id(randomInt(0, std::numeric_limits<decltype(answer.message_id())>::max()));
            ASSERT_FALSE(subscribedPredicate(answer));
        }
    }
}

TEST_F(CommunicatorTest, shouldAddHandshakeOnSetupHandshakeACK)
{
    std::function<std::unique_ptr<MockCommunicationHandler::Message>()> addedMetaHandshake;
    std::function<std::unique_ptr<MockCommunicationHandler::Message>()> addedDataHandshake;

    EXPECT_CALL(*handlerMock, addHandshake(_, MockCommunicationHandler::Pool::META)).
            WillOnce(DoAll(SaveArg<0>(&addedMetaHandshake), Return([]{})));

    EXPECT_CALL(*handlerMock, addHandshake(_, MockCommunicationHandler::Pool::DATA)).
            WillOnce(DoAll(SaveArg<0>(&addedDataHandshake), Return([]{})));

    communicator->setupHandshakeAck();

    one::clproto::fuse_messages::HandshakeAck ack;
    ASSERT_TRUE(ack.ParseFromString(addedMetaHandshake()->input()));
    ASSERT_EQ(fuseId, ack.fuse_id());

    ASSERT_TRUE(ack.ParseFromString(addedDataHandshake()->input()));
    ASSERT_EQ(fuseId, ack.fuse_id());
}


TEST_F(CommunicatorTest, shouldCallDataPoolOnSendingRemoteFileManagementMessages)
{
    one::clproto::remote_file_management::ChangePermsAtStorage msg;
    msg.set_file_id(randomString());
    msg.set_perms(666);

    EXPECT_CALL(*handlerMock, send(_, MockCommunicationHandler::Pool::DATA, _));
    communicator->send(randomModule, msg);
    Mock::VerifyAndClearExpectations(handlerMock);

    EXPECT_CALL(*handlerMock, communicateMock(_, MockCommunicationHandler::Pool::DATA));
    communicator->communicate(randomModule, msg, 0, std::chrono::milliseconds{0});
    Mock::VerifyAndClearExpectations(handlerMock);

    EXPECT_CALL(*handlerMock, communicateMock(_, MockCommunicationHandler::Pool::DATA));
    communicator->communicateAsync(randomModule, msg);
}


TEST_F(CommunicatorTest, shouldCallDataPoolOnSendingOtherMessages)
{
    one::clproto::fuse_messages::ChannelClose msg;
    msg.set_fuse_id(fuseId);

    EXPECT_CALL(*handlerMock, send(_, MockCommunicationHandler::Pool::META, _));
    communicator->send(randomModule, msg);
    Mock::VerifyAndClearExpectations(handlerMock);

    EXPECT_CALL(*handlerMock, communicateMock(_, MockCommunicationHandler::Pool::META));
    communicator->communicate(randomModule, msg, 0, std::chrono::milliseconds{0});
    Mock::VerifyAndClearExpectations(handlerMock);

    EXPECT_CALL(*handlerMock, communicateMock(_, MockCommunicationHandler::Pool::META));
    communicator->communicateAsync(randomModule, msg);
}

TEST_F(CommunicatorTest, shouldWrapAndPassMessagesOnSend)
{
    std::string module = one::communication::toString(randomModule);

    one::clproto::remote_file_management::RemoteFileMangement msg;
    msg.set_input(randomString());
    msg.set_message_type(randomString());

    one::clproto::communication_protocol::ClusterMsg wrapper;
    EXPECT_CALL(*handlerMock, send(_, _, _)).WillOnce(SaveArg<0>(&wrapper));
    communicator->send(randomModule, msg);

    ASSERT_EQ("remotefilemangement", wrapper.message_type());
    ASSERT_EQ("remote_file_management", wrapper.message_decoder_name());
    ASSERT_EQ("atom", wrapper.answer_type());
    ASSERT_EQ("communication_protocol", wrapper.answer_decoder_name());
    ASSERT_EQ(module, wrapper.module_name());
    ASSERT_FALSE(wrapper.synch());
    ASSERT_TRUE(wrapper.has_protocol_version());

    decltype(msg) sentMsg;
    ASSERT_TRUE(sentMsg.ParseFromString(wrapper.input()));
    ASSERT_EQ(msg.SerializeAsString(), sentMsg.SerializeAsString());
}

TEST_F(CommunicatorTest, shouldWrapAndPassMessagesOnCommunicate)
{
    std::string module = one::communication::toString(randomModule);

    one::clproto::fuse_messages::CreateDir msg;
    msg.set_dir_logic_name(randomString());
    msg.set_mode(666);

    one::clproto::communication_protocol::ClusterMsg wrapper;
    EXPECT_CALL(*handlerMock, communicateMock(_, _)).WillOnce(SaveArg<0>(&wrapper));
    communicator->communicate<one::clproto::fuse_messages::ChannelClose>(randomModule, msg, 0, std::chrono::milliseconds{0});

    ASSERT_EQ("createdir", wrapper.message_type());
    ASSERT_EQ("fuse_messages", wrapper.message_decoder_name());
    ASSERT_EQ("channelclose", wrapper.answer_type());
    ASSERT_EQ("fuse_messages", wrapper.answer_decoder_name());
    ASSERT_EQ(module, wrapper.module_name());
    ASSERT_TRUE(wrapper.synch());
    ASSERT_TRUE(wrapper.has_protocol_version());

    decltype(msg) sentMsg;
    ASSERT_TRUE(sentMsg.ParseFromString(wrapper.input()));
    ASSERT_EQ(msg.SerializeAsString(), sentMsg.SerializeAsString());
}

TEST_F(CommunicatorTest, shouldWrapAndPassMessagesOnCommunicateAsync)
{
    std::string module = one::communication::toString(randomModule);

    one::clproto::logging::ChangeRemoteLogLevel msg;
    msg.set_level(one::clproto::logging::INFO);

    one::clproto::communication_protocol::ClusterMsg wrapper;
    EXPECT_CALL(*handlerMock, communicateMock(_, _)).WillOnce(SaveArg<0>(&wrapper));
    communicator->communicateAsync<one::clproto::remote_file_management::DeleteFileAtStorage>(randomModule, msg);

    ASSERT_EQ("changeremoteloglevel", wrapper.message_type());
    ASSERT_EQ("logging", wrapper.message_decoder_name());
    ASSERT_EQ("deletefileatstorage", wrapper.answer_type());
    ASSERT_EQ("remote_file_management", wrapper.answer_decoder_name());
    ASSERT_EQ(module, wrapper.module_name());
    ASSERT_FALSE(wrapper.synch());
    ASSERT_TRUE(wrapper.has_protocol_version());

    decltype(msg) sentMsg;
    ASSERT_TRUE(sentMsg.ParseFromString(wrapper.input()));
    ASSERT_EQ(msg.SerializeAsString(), sentMsg.SerializeAsString());
}

TEST_F(CommunicatorTest, shouldAskForAtomAnswerByDefault)
{
    one::clproto::fuse_messages::ChannelRegistration msg;
    msg.set_fuse_id(fuseId);

    one::clproto::communication_protocol::ClusterMsg wrapper;
    EXPECT_CALL(*handlerMock, communicateMock(_, _)).WillRepeatedly(SaveArg<0>(&wrapper));

    communicator->communicate(randomModule, msg);
    ASSERT_EQ("atom", wrapper.answer_type());
    ASSERT_EQ("communication_protocol", wrapper.answer_decoder_name());

    communicator->communicateAsync(randomModule, msg);
    ASSERT_EQ("atom", wrapper.answer_type());
    ASSERT_EQ("communication_protocol", wrapper.answer_decoder_name());
}

TEST_F(CommunicatorTest, shouldWaitForAnswerOnCommunicate)
{
    handlerMock->autoFulfillPromise = false;
    std::atomic<bool> communicationDone{false};
    std::condition_variable statusChanged;

    auto fulfilPromise = [&]{
        std::this_thread::sleep_for(std::chrono::milliseconds{250});
        ASSERT_FALSE(communicationDone);
        handlerMock->promise->set_value({});

        std::mutex m;
        std::unique_lock<std::mutex> lock{m};
        statusChanged.wait_for(lock, std::chrono::seconds{5}, [&]{ return communicationDone.load(); });
        ASSERT_TRUE(communicationDone);
    };

    one::clproto::fuse_messages::ChannelRegistration msg;
    msg.set_fuse_id(fuseId);

    std::thread t{fulfilPromise};
    communicator->communicate(randomModule, msg, 0, std::chrono::seconds{20});
    communicationDone = true;
    statusChanged.notify_one();

    t.join();
}

TEST_F(CommunicatorTest, shouldThrowOnCommunicateReceiveTimeout)
{
    handlerMock->autoFulfillPromise = false;

    one::clproto::fuse_messages::ChannelRegistration msg;
    msg.set_fuse_id(fuseId);

    ASSERT_THROW(communicator->communicate(randomModule, msg, 0,std::chrono::seconds{0}),
                 one::communication::ReceiveError);
}

TEST_F(CommunicatorTest, shouldReturnAFullfilableFutureOnCommunicateAsync)
{
    handlerMock->autoFulfillPromise = false;

    one::clproto::fuse_messages::ChannelRegistration msg;
    msg.set_fuse_id(fuseId);
    auto future = communicator->communicateAsync(randomModule, msg);

    ASSERT_EQ(std::future_status::timeout, future.wait_for(std::chrono::seconds{0}));
    handlerMock->promise->set_value({});
    ASSERT_EQ(std::future_status::ready, future.wait_for(std::chrono::seconds{0}));
}

TEST_F(CommunicatorTest, shouldWrapAndPassMessagesOnReply)
{
    std::string module = one::communication::toString(randomModule);

    one::clproto::fuse_messages::ChannelClose msg;
    msg.set_fuse_id(fuseId);

    one::clproto::communication_protocol::Answer replyTo;
    replyTo.set_message_id(randomInt());

    one::clproto::communication_protocol::ClusterMsg wrapper;
    EXPECT_CALL(*handlerMock, reply(_, _, _, _)).WillOnce(SaveArg<1>(&wrapper));
    communicator->reply(replyTo, randomModule, msg);

    ASSERT_EQ("channelclose", wrapper.message_type());
    ASSERT_EQ("fuse_messages", wrapper.message_decoder_name());
    ASSERT_EQ("atom", wrapper.answer_type());
    ASSERT_EQ("communication_protocol", wrapper.answer_decoder_name());
    ASSERT_EQ(module, wrapper.module_name());
    ASSERT_FALSE(wrapper.synch());
    ASSERT_TRUE(wrapper.has_protocol_version());

    decltype(msg) sentMsg;
    ASSERT_TRUE(sentMsg.ParseFromString(wrapper.input()));
    ASSERT_EQ(msg.SerializeAsString(), sentMsg.SerializeAsString());
}

TEST_F(CommunicatorTest, shouldReplaceSubscriptionOnSetupPushChannel)
{
    using one::clproto::communication_protocol::Answer;

    bool unsubscribeCalled = false;
    bool rightCallbackCalled = false;

    std::function<bool(const Answer&)> savedPred;
    std::function<void(const Answer&)> savedCallback;

    EXPECT_CALL(*handlerMock, subscribe(_)).WillOnce(
                DoAll(SaveFunctions(&savedPred, &savedCallback), Return([]{})));
    EXPECT_CALL(*handlerMock, subscribe(_)).WillOnce(
                Return([&]{ unsubscribeCalled = true; })).RetiresOnSaturation();

    communicator->setupPushChannels([](const Answer&){});
    communicator->setupPushChannels([&](const Answer&){ rightCallbackCalled = true; });

    savedCallback(Answer{});

    ASSERT_TRUE(unsubscribeCalled);
    ASSERT_TRUE(rightCallbackCalled);
}

TEST_F(CommunicatorTest, shouldReplaceHandshakeOnSetupPushChannel)
{
    using one::clproto::communication_protocol::ClusterMsg;
    using one::clproto::communication_protocol::Answer;

    bool removeCalled = false;
    std::function<std::unique_ptr<ClusterMsg>()> handshake;

    EXPECT_CALL(*handlerMock, addHandshake(_, _, _)).WillOnce(
                DoAll(SaveArg<0>(&handshake), Return([]{})));
    EXPECT_CALL(*handlerMock, addHandshake(_, _, _)).WillOnce(
                Return([&]{ removeCalled = true; })).RetiresOnSaturation();

    communicator->setupPushChannels([](const Answer&){});

    auto expectedFuseId = randomString();
    communicator->setFuseId(expectedFuseId);
    communicator->setupPushChannels([](const Answer&){});

    ASSERT_TRUE(removeCalled);

    one::clproto::fuse_messages::ChannelRegistration reg;
    ASSERT_TRUE(reg.ParseFromString(handshake()->input()));
    ASSERT_EQ(expectedFuseId, reg.fuse_id());
}

TEST_F(CommunicatorTest, shouldReplaceHandshakeOnSetupHandshakeAck)
{
    using one::clproto::communication_protocol::ClusterMsg;
    using one::clproto::communication_protocol::Answer;

    int removeCalled = 0;
    std::function<std::unique_ptr<ClusterMsg>()> metaHandshake;
    std::function<std::unique_ptr<ClusterMsg>()> dataHandshake;

    EXPECT_CALL(*handlerMock, addHandshake(_, MockCommunicationHandler::Pool::META)).
            WillOnce(DoAll(SaveArg<0>(&metaHandshake), Return([]{})));
    EXPECT_CALL(*handlerMock, addHandshake(_, MockCommunicationHandler::Pool::META)).
            WillOnce(Return([&]{ ++removeCalled; })).RetiresOnSaturation();

    EXPECT_CALL(*handlerMock, addHandshake(_, MockCommunicationHandler::Pool::DATA)).
            WillOnce(DoAll(SaveArg<0>(&dataHandshake), Return([]{})));
    EXPECT_CALL(*handlerMock, addHandshake(_, MockCommunicationHandler::Pool::DATA)).
            WillOnce(Return([&]{ ++removeCalled; })).RetiresOnSaturation();

    communicator->setupHandshakeAck();

    const auto expectedFuseId = randomString();
    communicator->setFuseId(expectedFuseId); // calls setupHandshakeAck

    ASSERT_EQ(2, removeCalled);

    one::clproto::fuse_messages::HandshakeAck ack;

    ASSERT_TRUE(ack.ParseFromString(metaHandshake()->input()));
    ASSERT_EQ(expectedFuseId, ack.fuse_id());

    ASSERT_TRUE(ack.ParseFromString(dataHandshake()->input()));
    ASSERT_EQ(expectedFuseId, ack.fuse_id());
}
