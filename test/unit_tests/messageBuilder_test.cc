/**
 * @file messageBuilder_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "options_mock.h"
#include "jobScheduler_mock.h"
#include "options_mock.h"
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
    FuseMessage msg = proxy.createFuseMessage("id", "Type", "input");

    EXPECT_EQ("type", msg.message_type());
    EXPECT_EQ("input", msg.input());
}

TEST_F(MessageBuilderTest, createClusterMessage) {
    ClusterMsg msg = proxy.createClusterMessage("moduleName", "messageType", "answerType", "answerDecoderName", true, "input");

    EXPECT_EQ("moduleName", msg.module_name());
    EXPECT_EQ("messagetype", msg.message_type());
    EXPECT_EQ("answertype", msg.answer_type());
    EXPECT_EQ("answerdecodername", msg.answer_decoder_name());
    EXPECT_EQ(true, msg.synch());
    EXPECT_EQ("input", msg.input());
}

TEST_F(MessageBuilderTest, packFuseMessage) {
    ClusterMsg msg = proxy.packFuseMessage("messageType", "answerType", "answerDecoderName", "messageInput");

    EXPECT_EQ(FUSE_MESSAGE, msg.message_type());
    EXPECT_EQ("answertype", msg.answer_type());
    EXPECT_EQ("answerdecodername", msg.answer_decoder_name());
    EXPECT_EQ(FSLOGIC, msg.module_name());

    FuseMessage fMsg;
    fMsg.ParseFromString(msg.input());

    EXPECT_EQ("messagetype", fMsg.message_type());
    EXPECT_EQ("messageInput", fMsg.input());
}

TEST_F(MessageBuilderTest, decodeFuseAnswerNoWorkerAns) {
    Answer ans;
    FuseMessage msg = proxy.decodeFuseAnswer(ans);

    EXPECT_FALSE(msg.has_input());
}

TEST_F(MessageBuilderTest, decodeFuseAnswerWrongWorkerAns) {
    Answer ans;
    ans.set_worker_answer("wrong answer");
    FuseMessage msg = proxy.decodeFuseAnswer(ans);

    EXPECT_FALSE(msg.IsInitialized());
}


TEST_F(MessageBuilderTest, decodeFuseAnswerNormalAns) {
    Answer ans;
    FuseMessage fMsg =  proxy.createFuseMessage("id", "type", "input");
    ans.set_worker_answer(fMsg.SerializeAsString());

    FuseMessage msg = proxy.decodeFuseAnswer(ans);

    ASSERT_TRUE(msg.IsInitialized());

    EXPECT_EQ(fMsg.message_type(), msg.message_type());
    EXPECT_EQ(fMsg.input(), msg.input());
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
