/**
 * @file fslogicProxy_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "fslogicProxy_proxy.h"
#include "messageBuilder_mock.h"
#include "options_mock.h"
#include "jobScheduler_mock.h"

#include <google/protobuf/descriptor.h>

#include <memory>

using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

#define CMSG_FROM(X) MessageBuilder(context).packFuseMessage("messageType", "answerType", "decoderName", X.SerializeAsString());

// TEST definitions below

// Helper function used to constuct protobuf message (arg<1>) from another protobuf message (arg<0>)
void setupAnswerResponse(google::protobuf::Message& from, google::protobuf::Message& to) {
    to.ParsePartialFromString(from.SerializePartialAsString());
}

class FslogicProxyTest
    : public ::testing::Test
{
protected:
    COMMON_DEFS();
    std::unique_ptr<ProxyFslogicProxy> proxy;
    boost::shared_ptr<MockMessageBuilder> msgBuilder;

    ClusterMsg fullClusterMsg;

    virtual void SetUp() {
        COMMON_SETUP();

        proxy.reset(new ProxyFslogicProxy{context});

        EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(true));

        msgBuilder.reset(new MockMessageBuilder(context));
        proxy->setMessageBuilder(msgBuilder);
        proxy->mockAtom = false;
        proxy->mockAnswer = false;
        proxy->ch_mock.reset(new MockCommunicationHandler());
        EXPECT_CALL(*connectionPool, selectConnection(_)).WillRepeatedly(Return(proxy->ch_mock));
        EXPECT_CALL(*connectionPool, releaseConnection(_)).WillRepeatedly(Return());

    }

    virtual void TearDown() {
        COMMON_CLEANUP();
    }

};

TEST_F(FslogicProxyTest, sendFuseReceiveAnswerFails) {
    GetFileChildren msg;
    msg.set_dir_logic_name("dir");
    msg.set_children_num(10);
    msg.set_offset(0);
    FileChildren answer;

    ClusterMsg cMsg = CMSG_FROM(msg);

    EXPECT_CALL(*msgBuilder, packFuseMessage(StrCaseEq(msg.GetDescriptor()->name()), StrCaseEq(answer.GetDescriptor()->name()), FUSE_MESSAGES, msg.SerializeAsString())).WillOnce(Return(ClusterMsg()));
    EXPECT_FALSE(proxy->sendFuseReceiveAnswer(msg, answer));


    EXPECT_CALL(*msgBuilder, packFuseMessage(StrCaseEq(msg.GetDescriptor()->name()), StrCaseEq(answer.GetDescriptor()->name()), FUSE_MESSAGES, msg.SerializeAsString())).WillRepeatedly(Return(cMsg));

    Answer ans;
    ans.set_answer_status("not ok");
    EXPECT_CALL(*proxy->ch_mock, communicate(Truly(bind(pbMessageEqual, cMsg, _1)), _, _)).WillOnce(Return(ans));

    EXPECT_FALSE(proxy->sendFuseReceiveAnswer(msg, answer));
}

TEST_F(FslogicProxyTest, sendFuseReceiveAnswerOK) {
    GetFileChildren msg;
    msg.set_dir_logic_name("dir");
    msg.set_children_num(10);
    msg.set_offset(0);
    FileChildren answer;

    ClusterMsg cMsg = CMSG_FROM(msg);

    EXPECT_CALL(*msgBuilder, packFuseMessage(StrCaseEq(msg.GetDescriptor()->name()), StrCaseEq(answer.GetDescriptor()->name()), FUSE_MESSAGES, msg.SerializeAsString())).WillRepeatedly(Return(cMsg));

    Answer ans;
    FileChildren response;
    response.add_child_logic_name("cos");
    ans.set_answer_status(VOK);
    ans.set_worker_answer(response.SerializeAsString());
    EXPECT_CALL(*proxy->ch_mock, communicate(Truly(bind(pbMessageEqual, cMsg, _1)), _, _)).WillOnce(Return(ans));

    EXPECT_TRUE(proxy->sendFuseReceiveAnswer(msg, answer));
    EXPECT_EQ(response.SerializeAsString(), answer.SerializeAsString());
}

TEST_F(FslogicProxyTest, sendFuseReceiveAtomFails) {
    GetFileChildren msg;
    msg.set_dir_logic_name("dir");
    msg.set_children_num(10);
    msg.set_offset(0);

    ClusterMsg cMsg = CMSG_FROM(msg);

    EXPECT_CALL(*msgBuilder, packFuseMessage(StrCaseEq(msg.GetDescriptor()->name()), StrCaseEq(Atom::descriptor()->name()), COMMUNICATION_PROTOCOL, msg.SerializeAsString())).WillOnce(Return(ClusterMsg()));
    EXPECT_EQ(VEIO, proxy->sendFuseReceiveAtom(msg));


    EXPECT_CALL(*msgBuilder, packFuseMessage(StrCaseEq(msg.GetDescriptor()->name()), StrCaseEq(Atom::descriptor()->name()), COMMUNICATION_PROTOCOL, msg.SerializeAsString())).WillRepeatedly(Return(cMsg));

    Answer ans;
    ans.set_answer_status("not ok");
    EXPECT_CALL(*proxy->ch_mock, communicate(Truly(bind(pbMessageEqual, cMsg, _1)), _, _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(""));
    EXPECT_EQ(VEIO, proxy->sendFuseReceiveAtom(msg));
}

TEST_F(FslogicProxyTest, sendFuseReceiveAtomOK) {
    GetFileChildren msg;
    msg.set_dir_logic_name("dir");
    msg.set_children_num(10);
    msg.set_offset(0);

    ClusterMsg cMsg = CMSG_FROM(msg);

    EXPECT_CALL(*msgBuilder, packFuseMessage(StrCaseEq(msg.GetDescriptor()->name()), StrCaseEq(Atom::descriptor()->name()), COMMUNICATION_PROTOCOL, msg.SerializeAsString())).WillRepeatedly(Return(cMsg));

    Answer ans;
    Atom response;
    response.set_value("value");
    ans.set_answer_status(VOK);
    ans.set_worker_answer(response.SerializeAsString());
    EXPECT_CALL(*proxy->ch_mock, communicate(Truly(bind(pbMessageEqual, cMsg, _1)), _, _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(Truly(bind(pbMessageEqual, ans, _1)))).WillOnce(Return("value"));
    EXPECT_EQ("value", proxy->sendFuseReceiveAtom(msg));
}

TEST_F(FslogicProxyTest, getFileAttr) {
    proxy->mockAnswer = true;
    GetFileAttr msg;
    msg.set_file_logic_name("/file");

    FileAttr attributes;
    FileAttr response;


    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, response, _1) )), Return(false)));
    EXPECT_FALSE(proxy->getFileAttr("/file", response));


    attributes.set_atime(0);
    attributes.set_mtime(0);
    attributes.set_ctime(0);
    attributes.set_gid(1);
    attributes.set_uid(2);
    attributes.set_mode(1234);
    attributes.set_type("type");

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, attributes, _1) )), Return(true)));
    ASSERT_TRUE(proxy->getFileAttr("/file", response));


    EXPECT_EQ(attributes.mode(), response.mode());
    EXPECT_EQ(attributes.type(), response.type());
}

TEST_F(FslogicProxyTest, getFileLocation) {
    proxy->mockAnswer = true;
    GetFileLocation msg;
    msg.set_file_logic_name("/file");

    FileLocation location;
    FileLocation response;

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, response, _1) )), Return(false)));
    EXPECT_FALSE(proxy->getFileLocation("/file", response));

    location.set_validity(10);
    location.set_answer(VEACCES);
    location.set_storage_id(4);
    location.set_file_id("fileid");

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, location, _1) )), Return(true)));
    ASSERT_TRUE(proxy->getFileLocation("/file", response));

    EXPECT_EQ(location.validity(), response.validity());
    EXPECT_EQ(location.answer(), response.answer());
}

TEST_F(FslogicProxyTest, getNewFileLocation) {
    proxy->mockAnswer = true;
    GetNewFileLocation msg;
    msg.set_file_logic_name("/file");
    msg.set_mode(234);

    FileLocation location;
    FileLocation response;

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, response, _1) )), Return(false)));
    EXPECT_FALSE(proxy->getNewFileLocation("/file", 234, response));


    location.set_validity(10);
    location.set_answer(VEACCES);
    location.set_storage_id(4);
    location.set_file_id("fileid");

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, location, _1) )), Return(true)));
    ASSERT_TRUE(proxy->getNewFileLocation("/file", 234, response));


    EXPECT_EQ(location.validity(), response.validity());
    EXPECT_EQ(location.answer(), response.answer());

}

TEST_F(FslogicProxyTest, sendFileCreatedAck) {
    proxy->mockAtom = true;
    CreateFileAck msg;
    msg.set_file_logic_name("/file");

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->sendFileCreatedAck("/file"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->sendFileCreatedAck("/file"));
}

TEST_F(FslogicProxyTest, renewFileLocation) {
    proxy->mockAnswer = true;
    RenewFileLocation msg;
    FileLocationValidity validity;
    msg.set_file_logic_name("/file");

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, validity, _1) )), Return(false)));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, validity, _1) )), Return(true)));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    validity.set_answer(VOK);
    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, validity, _1) )), Return(true)));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    validity.set_answer(VOK);
    validity.set_validity(-1);
    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, validity, _1) )), Return(true)));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    validity.set_answer(VEACCES);
    validity.set_validity(10);
    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, validity, _1) )), Return(true)));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));


    validity.set_answer(VOK);
    validity.set_validity(15);
    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, validity, _1) )), Return(true)));
    EXPECT_EQ(15, proxy->renewFileLocation("/file"));

}

TEST_F(FslogicProxyTest, getFileChildren) {
    proxy->mockAnswer = true;
    std::vector<std::string> childrenVect;
    GetFileChildren msg;
    msg.set_dir_logic_name("/dir");
    msg.set_children_num(10);
    msg.set_offset(5);

    FileChildren children;

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, children, _1) )), Return(false)));
    EXPECT_FALSE(proxy->getFileChildren("/dir", 10, 5, childrenVect));

    childrenVect.clear();
    FileChildren response;
    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, response, _1) )), Return(true)));
    EXPECT_TRUE(proxy->getFileChildren("/dir", 10, 5, childrenVect));
    EXPECT_EQ(0u, childrenVect.size());

    response.add_child_logic_name("/child2");
    response.add_child_logic_name("/child1");
    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, response, _1) )), Return(true)));
    EXPECT_TRUE(proxy->getFileChildren("/dir", 10, 5, childrenVect));
    EXPECT_EQ(2u, childrenVect.size());
    EXPECT_EQ("/child2", childrenVect[0]);
    EXPECT_EQ("/child1", childrenVect[1]);

}

TEST_F(FslogicProxyTest, createDir) {
    proxy->mockAtom = true;

    CreateDir msg;
    msg.set_dir_logic_name("/dir");
    msg.set_mode(1234);

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->createDir("/dir", 1234));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->createDir("/dir", 1234));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->createDir("/dir", 1234));
}

TEST_F(FslogicProxyTest, deleteFile) {
    proxy->mockAtom = true;

    DeleteFile msg;
    msg.set_file_logic_name("/path");

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->deleteFile("/path"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->deleteFile("/path"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->deleteFile("/path"));
}

TEST_F(FslogicProxyTest, sendFileNotUsed) {
    proxy->mockAtom = true;

    FileNotUsed msg;
    msg.set_file_logic_name("/path");

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_FALSE(proxy->sendFileNotUsed("/path"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_TRUE(proxy->sendFileNotUsed("/path"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_FALSE(proxy->sendFileNotUsed("/path"));
}

TEST_F(FslogicProxyTest, renameFile) {
    proxy->mockAtom = true;

    RenameFile msg;
    msg.set_from_file_logic_name("/path");
    msg.set_to_file_logic_name("/new/path");

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->renameFile("/path", "/new/path"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->renameFile("/path", "/new/path"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->renameFile("/path", "/new/path"));
}

TEST_F(FslogicProxyTest, changeFilePerms) {
    proxy->mockAtom = true;

    ChangeFilePerms msg;
    msg.set_file_logic_name("/path");
    msg.set_perms(123);

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->changeFilePerms("/path", 123));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->changeFilePerms("/path", 123));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->changeFilePerms("/path", 123));

}

TEST_F(FslogicProxyTest, createLink) {
    proxy->mockAtom = true;

    CreateLink msg;
    msg.set_from_file_logic_name("/from");
    msg.set_to_file_logic_name("/to");

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->createLink("/from", "/to"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->createLink("/from", "/to"));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->createLink("/from", "/to"));
}

TEST_F(FslogicProxyTest, getLink) {
    proxy->mockAnswer = true;

    GetLink msg;
    msg.set_file_logic_name("/from");

    LinkInfo response;

    pair<string, string> resp;

    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, response, _1) )), Return(false)));
    resp = proxy->getLink("/from");
    EXPECT_EQ(VEIO, resp.first);

    response.set_file_logic_name("/to");
    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, response, _1) )), Return(true)));
    resp = proxy->getLink("/from");
    EXPECT_EQ(VOK, resp.first);
    EXPECT_EQ("/to", resp.second);

    response.set_answer(VEACCES);
    response.set_file_logic_name("");
    EXPECT_CALL(*proxy, mockAnswerFun( Truly(bind(pbMessageEqual, msg, _1)), _ ) ).WillOnce(DoAll(WithArgs<1>(Invoke( bind(setupAnswerResponse, response, _1) )), Return(true)));
    resp = proxy->getLink("/from");
    EXPECT_EQ(VEACCES, resp.first);

}


TEST_F(FslogicProxyTest, updateTimes) {
    proxy->mockAtom = true;

    UpdateTimes msg;
    msg.set_atime(10);
    msg.set_mtime(11);
    msg.set_ctime(12);
    msg.set_file_logic_name("/path");

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->updateTimes("/path", 10, 11, 12));

    msg.clear_ctime();
    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->updateTimes("/path", 10, 11));

    msg.set_ctime(13);
    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->updateTimes("/path", 10, 11, 13));
}

TEST_F(FslogicProxyTest, changeFileOwner) {
    proxy->mockAtom = true;

    ChangeFileOwner msg;
    msg.set_file_logic_name("/path");
    msg.set_uid(456);

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->changeFileOwner("/path", 456));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->changeFileOwner("/path", 456));

    msg.set_uname("username");
    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->changeFileOwner("/path", 456, "username"));
}

TEST_F(FslogicProxyTest, changeFileGroup) {
    proxy->mockAtom = true;

    ChangeFileGroup msg;
    msg.set_file_logic_name("/path");
    msg.set_gid(456);

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->changeFileGroup("/path", 456));

    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->changeFileGroup("/path", 456));

    msg.set_gname("groupname");
    EXPECT_CALL(*proxy, mockAtomFun(Truly(bind(pbMessageEqual, msg, _1)))).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->changeFileGroup("/path", 456, "groupname"));
}
