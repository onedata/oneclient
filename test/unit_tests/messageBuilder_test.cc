/**
 * @file messageBuilder_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */


#include "messageBuilder.h"

#include "fslogicProxy.h"
#include "make_unique.h"
#include "options_mock.h"
#include "testCommon.h"
#include "testUtils.h"

using namespace ::testing;
using namespace veil::client;
using namespace veil::protocol::communication_protocol;
using namespace veil::protocol::fuse_messages;
using veil::FUSE_MESSAGE;

class MessageBuilderTest: public CommonTest
{
protected:
    std::unique_ptr<MessageBuilder> proxy;

    void SetUp() override
    {
        CommonTest::SetUp();
        proxy = std::make_unique<MessageBuilder>(context);
    }
};

TEST_F(MessageBuilderTest, createFuseMessage) {
    GetFileLocation loc;
    loc.set_file_logic_name(randomString());
    FuseMessage msg = proxy->createFuseMessage(loc);

    EXPECT_EQ("getfilelocation", msg.message_type());
    EXPECT_EQ(loc.SerializeAsString(), msg.input());
}

TEST_F(MessageBuilderTest, decodeFuseAnswerNoWorkerAns) {
    Answer ans;
    FuseMessage msg = proxy->decodeFuseAnswer(ans);

    EXPECT_FALSE(msg.has_input());
}

TEST_F(MessageBuilderTest, decodeFuseAnswerWrongWorkerAns) {
    Answer ans;
    ans.set_worker_answer("wrong answer");
    FuseMessage msg = proxy->decodeFuseAnswer(ans);

    EXPECT_FALSE(msg.IsInitialized());
}


TEST_F(MessageBuilderTest, decodeFuseAnswerNormalAns) {
    GetFileLocation loc;
    loc.set_file_logic_name(randomString());
    FuseMessage fMsg = proxy->createFuseMessage(loc);

    Answer ans;
    ans.set_worker_answer(fMsg.SerializeAsString());

    FuseMessage msg = proxy->decodeFuseAnswer(ans);
    ASSERT_TRUE(msg.IsInitialized());

    EXPECT_EQ(fMsg.message_type(), msg.message_type());
    EXPECT_EQ(fMsg.input(), msg.input());
}

TEST_F(MessageBuilderTest, decodeAtomAnswerWrongInput) {
    Answer ans;
    ans.set_worker_answer("wrong input");
    std::string msg = proxy->decodeAtomAnswer(ans);

    EXPECT_EQ("", msg);
}

TEST_F(MessageBuilderTest, decodeAtomAnswerNoInput) {
    Answer ans;
    std::string msg = proxy->decodeAtomAnswer(ans);

    EXPECT_EQ("", msg);
}

TEST_F(MessageBuilderTest, decodeAtomAnswerNormalInput) {
    Answer ans;
    Atom a;
    a.set_value("value");
    ans.set_worker_answer(a.SerializeAsString());
    std::string msg = proxy->decodeAtomAnswer(ans);

    EXPECT_EQ("value", msg);
}
