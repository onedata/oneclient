/**
 * @file messageBuilder_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "config_mock.h"
#include "jobScheduler_mock.h"
#include "config_mock.h"
#include "messageBuilder.h"

using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class MessageBuilderTest 
    : public ::testing::Test {
protected:
    COMMON_DEFS();
    MessageBuilder proxy;     

    virtual void SetUp() {
        COMMON_SETUP();
    }

    virtual void TearDown() {
        COMMON_CLEANUP();
    }

};

TEST_F(MessageBuilderTest, createFuseMessage) {
    FuseMessage *msg = proxy.createFuseMessage("id", "type", "input");

    EXPECT_EQ("id", msg->id());
    EXPECT_EQ("type", msg->message_type());
    EXPECT_EQ("input", msg->input());
    
    delete msg;
}

TEST_F(MessageBuilderTest, createClusterMessage) {
    ClusterMsg *msg = proxy.createClusterMessage("moduleName", "messageType", "answerType", "answerDecoderName", true, "input");
    
    EXPECT_EQ("moduleName", msg->module_name());
    EXPECT_EQ("messageType", msg->message_type());
    EXPECT_EQ("answerType", msg->answer_type());
    EXPECT_EQ("answerDecoderName", msg->answer_decoder_name());
    EXPECT_EQ(true, msg->synch());
    EXPECT_EQ("input", msg->input());

    delete msg;
}

TEST_F(MessageBuilderTest, packFuseMessageWithPresetFuseId) {
    EXPECT_CALL(*config, isSet(FUSE_ID_OPT)).WillOnce(Return(true));
    EXPECT_CALL(*config, getString(FUSE_ID_OPT)).WillOnce(Return("FUSE_ID"));

    ClusterMsg *msg = proxy.packFuseMessage("messageType", "answerType", "answerDecoderName", "messageInput");

    EXPECT_EQ(FUSE_MESSAGE, msg->message_type());
    EXPECT_EQ("answerType", msg->answer_type());
    EXPECT_EQ("answerDecoderName", msg->answer_decoder_name());
    EXPECT_EQ(FSLOGIC, msg->module_name());

    FuseMessage fMsg;
    fMsg.ParseFromString(msg->input());

    EXPECT_EQ("messageType", fMsg.message_type());
    EXPECT_EQ("FUSE_ID", fMsg.id());
    EXPECT_EQ("messageInput", fMsg.input());

    delete msg;
}

TEST_F(MessageBuilderTest, packFuseMessageWithoutPresetFuseId) {
    EXPECT_CALL(*config, isSet(FUSE_ID_OPT)).WillOnce(Return(false));

    ClusterMsg *msg = proxy.packFuseMessage("messageType", "answerType", "answerDecoderName", "messageInput");
    
    EXPECT_EQ(FUSE_MESSAGE, msg->message_type());
    EXPECT_EQ("answerType", msg->answer_type());
    EXPECT_EQ("answerDecoderName", msg->answer_decoder_name());
    EXPECT_EQ(FSLOGIC, msg->module_name());

    FuseMessage fMsg;
    fMsg.ParseFromString(msg->input());

    EXPECT_EQ("messageType", fMsg.message_type());
    EXPECT_NE("FUSE_ID", fMsg.id());
    EXPECT_EQ("messageInput", fMsg.input());

    delete msg;
}

TEST_F(MessageBuilderTest, decodeFuseAnswerNoWorkerAns) {    
    Answer ans;
    FuseMessage *msg = proxy.decodeFuseAnswer(ans);

    EXPECT_EQ(NULL, msg);

    delete msg; // Deleting NULL pointer should be safe 
}

TEST_F(MessageBuilderTest, decodeFuseAnswerWrongWorkerAns) {    
    Answer ans;
    ans.set_worker_answer("wrong answer");
    FuseMessage *msg = proxy.decodeFuseAnswer(ans);

    EXPECT_EQ(NULL, msg);

    delete msg; // Deleting NULL pointer should be safe 
}


TEST_F(MessageBuilderTest, decodeFuseAnswerNormalAns) {    
    Answer ans;
    FuseMessage *fMsg =  proxy.createFuseMessage("id", "type", "input"); 
    ans.set_worker_answer(fMsg->SerializeAsString());

    FuseMessage *msg = proxy.decodeFuseAnswer(ans);

    ASSERT_FALSE(NULL == msg);

    EXPECT_EQ(fMsg->id(), msg->id());
    EXPECT_EQ(fMsg->message_type(), msg->message_type());
    EXPECT_EQ(fMsg->input(), msg->input());
 
    delete fMsg;
    delete msg;
}

TEST_F(MessageBuilderTest, decodeAtomAnswerWrongInput) {
    Answer ans;
    ans.set_worker_answer("wrong input");
    string msg = proxy.decodeAtomAnswer(ans);

    EXPECT_EQ("", msg);
}

TEST_F(MessageBuilderTest, decodeAtomAnswerNoInput) {
    Answer ans;
    string msg = proxy.decodeAtomAnswer(ans);

    EXPECT_EQ("", msg);
}

TEST_F(MessageBuilderTest, decodeAtomAnswerNormalInput) {
    Answer ans;
    Atom a;
    a.set_value("value");
    ans.set_worker_answer(a.SerializeAsString());
    string msg = proxy.decodeAtomAnswer(ans);

    EXPECT_EQ("value", msg);
}