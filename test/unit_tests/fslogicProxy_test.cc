/**
 * @file fslogicProxy_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "fslogicProxy.h"

#include "communication/communicator_mock.h"
#include "jobScheduler_mock.h"
#include "make_unique.h"
#include "messageBuilder_mock.h"
#include "options_mock.h"
#include "testCommon.h"

#include <boost/algorithm/string/case_conv.hpp>
#include <google/protobuf/descriptor.h>
#include <google/protobuf/message.h>

#include <functional>
#include <memory>

using namespace ::testing;
using namespace std::placeholders;
using namespace veil::client;
using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

bool pbMessageEqual( const google::protobuf::MessageLite &lhs, const google::protobuf::MessageLite &rhs ) { return lhs.SerializePartialAsString() == rhs.SerializePartialAsString(); }

// Helper function used to constuct protobuf message (arg<1>) from another protobuf message (arg<0>)
void setupAnswerResponse(google::protobuf::Message& from, google::protobuf::Message& to) {
    to.ParsePartialFromString(from.SerializePartialAsString());
}

struct ProxyFslogicProxy: public veil::client::FslogicProxy
{
    using veil::client::FslogicProxy::FslogicProxy;
    using veil::client::FslogicProxy::m_messageBuilder;
    using veil::client::FslogicProxy::sendFuseReceiveAnswer;
    using veil::client::FslogicProxy::sendFuseReceiveAtom;
};

class FslogicProxyTest: public CommonTest
{
protected:
    std::unique_ptr<ProxyFslogicProxy> proxy;
    const MockMessageBuilder *msgBuilder;

    ClusterMsg fullClusterMsg;

    void SetUp() override {
        CommonTest::SetUp();

        proxy = std::make_unique<ProxyFslogicProxy>(context);

        auto p = std::make_unique<NiceMock<MockMessageBuilder>>();
        msgBuilder = p.get();
        proxy->m_messageBuilder = std::move(p);

        EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, get_fuse_id()).WillRepeatedly(Return("testID"));
        ON_CALL(*communicator, communicateMock(_, _, _, _)).WillByDefault(Return(Answer{}));
        ON_CALL(*msgBuilder, createFuseMessage(_)).WillByDefault(Return(FuseMessage{}));
    }

    veil::protocol::fuse_messages::FuseMessage fmsgFrom(const google::protobuf::Message &msg)
    {
        return MessageBuilder{}.createFuseMessage(msg);
    }
};

TEST_F(FslogicProxyTest, sendFuseReceiveAnswerFails) {
    GetFileChildren msg;
    msg.set_dir_logic_name("dir");
    msg.set_children_num(10);
    msg.set_offset(0);
    FileChildren answer;

    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillOnce(Return(FuseMessage{}));
    EXPECT_FALSE(proxy->sendFuseReceiveAnswer(msg, answer));

    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillRepeatedly(Return(fmsgFrom(msg)));

    Answer ans;
    ans.set_answer_status("not ok");
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));

    EXPECT_FALSE(proxy->sendFuseReceiveAnswer(msg, answer));
}

TEST_F(FslogicProxyTest, sendFuseReceiveAnswerOK) {
    GetFileChildren msg;
    msg.set_dir_logic_name("dir");
    msg.set_children_num(10);
    msg.set_offset(0);
    FileChildren answer;

    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillRepeatedly(Return(fmsgFrom(msg)));

    Answer ans;
    FileChildren response;
    response.add_child_logic_name("cos");
    ans.set_answer_status(VOK);
    ans.set_worker_answer(response.SerializeAsString());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));

    EXPECT_TRUE(proxy->sendFuseReceiveAnswer(msg, answer));
    EXPECT_EQ(response.SerializeAsString(), answer.SerializeAsString());
}

TEST_F(FslogicProxyTest, sendFuseReceiveAtomFails) {
    GetFileChildren msg;
    msg.set_dir_logic_name("dir");
    msg.set_children_num(10);
    msg.set_offset(0);

    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillOnce(Return(FuseMessage{}));
    EXPECT_EQ(VEIO, proxy->sendFuseReceiveAtom(msg));

    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillRepeatedly(Return(fmsgFrom(msg)));

    Answer ans;
    ans.set_answer_status("not ok");
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(""));
    EXPECT_EQ(VEIO, proxy->sendFuseReceiveAtom(msg));
}

TEST_F(FslogicProxyTest, sendFuseReceiveAtomOK) {
    GetFileChildren msg;
    msg.set_dir_logic_name("dir");
    msg.set_children_num(10);
    msg.set_offset(0);

    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillRepeatedly(Return(fmsgFrom(msg)));

    Answer ans;
    Atom response;
    response.set_value("value");
    ans.set_answer_status(VOK);
    ans.set_worker_answer(response.SerializeAsString());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(Truly(bind(pbMessageEqual, ans, _1)))).WillOnce(Return("value"));
    EXPECT_EQ("value", proxy->sendFuseReceiveAtom(msg));
}

TEST_F(FslogicProxyTest, getFileAttr) {
    GetFileAttr msg;
    msg.set_file_logic_name("/file");

    FileAttr attributes;
    FileAttr response;

    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(Answer{}));
    EXPECT_FALSE(proxy->getFileAttr("/file", response));


    attributes.set_atime(0);
    attributes.set_mtime(0);
    attributes.set_ctime(0);
    attributes.set_gid(1);
    attributes.set_uid(2);
    attributes.set_mode(1234);
    attributes.set_type("type");

    Answer ans;
    ans.set_answer_status(VOK);
    attributes.SerializePartialToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillOnce(Return(fmsgFrom(msg)));

    ASSERT_TRUE(proxy->getFileAttr("/file", response));

    EXPECT_EQ(attributes.mode(), response.mode());
    EXPECT_EQ(attributes.type(), response.type());
}

TEST_F(FslogicProxyTest, getFileLocation) {
    GetFileLocation msg;
    msg.set_file_logic_name("/file");
    msg.set_open_mode(UNSPECIFIED_MODE);

    FileLocation location;
    FileLocation response;

    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(Answer{}));
    EXPECT_FALSE(proxy->getFileLocation("/file", response,UNSPECIFIED_MODE));

    location.set_validity(10);
    location.set_answer(VEACCES);
    location.set_storage_id(4);
    location.set_file_id("fileid");

    Answer ans;
    ans.set_answer_status(VOK);
    location.SerializePartialToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillOnce(Return(fmsgFrom(msg)));
    ASSERT_TRUE(proxy->getFileLocation("/file", response,UNSPECIFIED_MODE));

    EXPECT_EQ(location.validity(), response.validity());
    EXPECT_EQ(location.answer(), response.answer());
}

TEST_F(FslogicProxyTest, getNewFileLocation) {
    GetNewFileLocation msg;
    msg.set_file_logic_name("/file");
    msg.set_mode(234);

    FileLocation location;
    FileLocation response;

    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(Answer{}));
    EXPECT_FALSE(proxy->getNewFileLocation("/file", 234, response));


    location.set_validity(10);
    location.set_answer(VEACCES);
    location.set_storage_id(4);
    location.set_file_id("fileid");

    Answer ans;
    ans.set_answer_status(VOK);
    location.SerializePartialToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, createFuseMessage(_)).WillOnce(Return(fmsgFrom(msg)));
    ASSERT_TRUE(proxy->getNewFileLocation("/file", 234, response));


    EXPECT_EQ(location.validity(), response.validity());
    EXPECT_EQ(location.answer(), response.answer());

}

TEST_F(FslogicProxyTest, sendFileCreatedAck) {
    CreateFileAck msg;
    msg.set_file_logic_name("/file");

    Answer answer;
    answer.set_answer_status(VOK);

    Atom atom;
    atom.set_value(VEIO);
    atom.SerializeToString(answer.mutable_worker_answer());

    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(answer));
    EXPECT_EQ(VEIO, proxy->sendFileCreatedAck("/file"));

    atom.set_value(VOK);
    atom.SerializeToString(answer.mutable_worker_answer());

    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(answer));
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->sendFileCreatedAck("/file"));
}

TEST_F(FslogicProxyTest, renewFileLocation) {
    RenewFileLocation msg;
    FileLocationValidity validity;
    msg.set_file_logic_name("/file");

    Answer ans;
    ans.set_answer_status(VOK);

    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(Answer{}));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    validity.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    validity.set_answer(VOK);
    validity.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    validity.set_answer(VOK);
    validity.set_validity(-1);
    validity.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    validity.set_answer(VEACCES);
    validity.set_validity(10);
    validity.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_GT(0, proxy->renewFileLocation("/file"));

    validity.set_answer(VOK);
    validity.set_validity(15);
    validity.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_EQ(15, proxy->renewFileLocation("/file"));
}

TEST_F(FslogicProxyTest, getFileChildren) {
    std::vector<std::string> childrenVect;
    GetFileChildren msg;
    msg.set_dir_logic_name("/dir");
    msg.set_children_num(10);
    msg.set_offset(5);

    FileChildren children;

    Answer ans;
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_FALSE(proxy->getFileChildren("/dir", 10, 5, childrenVect));

    childrenVect.clear();
    FileChildren response;
    ans.set_answer_status(VOK);
    response.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_TRUE(proxy->getFileChildren("/dir", 10, 5, childrenVect));
    EXPECT_EQ(0u, childrenVect.size());

    response.add_child_logic_name("/child2");
    response.add_child_logic_name("/child1");
    response.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    EXPECT_TRUE(proxy->getFileChildren("/dir", 10, 5, childrenVect));
    EXPECT_EQ(2u, childrenVect.size());
    EXPECT_EQ("/child2", childrenVect[0]);
    EXPECT_EQ("/child1", childrenVect[1]);

}

TEST_F(FslogicProxyTest, createDir) {
    CreateDir msg;
    msg.set_dir_logic_name("/dir");
    msg.set_mode(1234);

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->createDir("/dir", 1234));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->createDir("/dir", 1234));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->createDir("/dir", 1234));
}

TEST_F(FslogicProxyTest, deleteFile) {
    DeleteFile msg;
    msg.set_file_logic_name("/path");

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->deleteFile("/path"));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->deleteFile("/path"));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->deleteFile("/path"));
}

TEST_F(FslogicProxyTest, sendFileNotUsed) {
    FileNotUsed msg;
    msg.set_file_logic_name("/path");

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_FALSE(proxy->sendFileNotUsed("/path"));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_TRUE(proxy->sendFileNotUsed("/path"));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_FALSE(proxy->sendFileNotUsed("/path"));
}

TEST_F(FslogicProxyTest, renameFile) {
    RenameFile msg;
    msg.set_from_file_logic_name("/path");
    msg.set_to_file_logic_name("/new/path");

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->renameFile("/path", "/new/path"));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->renameFile("/path", "/new/path"));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->renameFile("/path", "/new/path"));
}

TEST_F(FslogicProxyTest, changeFilePerms) {
    ChangeFilePerms msg;
    msg.set_file_logic_name("/path");
    msg.set_perms(123);

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->changeFilePerms("/path", 123));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->changeFilePerms("/path", 123));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->changeFilePerms("/path", 123));

}

TEST_F(FslogicProxyTest, createLink) {
    CreateLink msg;
    msg.set_from_file_logic_name("/from");
    msg.set_to_file_logic_name("/to");

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->createLink("/from", "/to"));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->createLink("/from", "/to"));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->createLink("/from", "/to"));
}

TEST_F(FslogicProxyTest, getLink) {
    GetLink msg;
    msg.set_file_logic_name("/from");

    LinkInfo response;

    std::pair<std::string, std::string> resp;

    Answer ans;
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    resp = proxy->getLink("/from");
    EXPECT_EQ(VEIO, resp.first);

    ans.set_answer_status(VOK);
    response.set_file_logic_name("/to");
    response.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    resp = proxy->getLink("/from");
    EXPECT_EQ(VOK, resp.first);
    EXPECT_EQ("/to", resp.second);

    response.set_answer(VEACCES);
    response.set_file_logic_name("");
    response.set_file_logic_name("/to");
    response.SerializeToString(ans.mutable_worker_answer());
    EXPECT_CALL(*communicator, communicateMock(_, _, _, _)).WillOnce(Return(ans));
    resp = proxy->getLink("/from");
    EXPECT_EQ(VEACCES, resp.first);
}


TEST_F(FslogicProxyTest, updateTimes) {
    UpdateTimes msg;
    msg.set_atime(10);
    msg.set_mtime(11);
    msg.set_ctime(12);
    msg.set_file_logic_name("/path");

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->updateTimes("/path", 10, 11, 12));

    msg.clear_ctime();
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->updateTimes("/path", 10, 11));

    msg.set_ctime(13);
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->updateTimes("/path", 10, 11, 13));
}

TEST_F(FslogicProxyTest, changeFileOwner) {
    ChangeFileOwner msg;
    msg.set_file_logic_name("/path");
    msg.set_uid(456);

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->changeFileOwner("/path", 456));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->changeFileOwner("/path", 456));

    msg.set_uname("username");
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->changeFileOwner("/path", 456, "username"));
}

TEST_F(FslogicProxyTest, changeFileGroup) {
    ChangeFileGroup msg;
    msg.set_file_logic_name("/path");
    msg.set_gid(456);

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEIO));
    EXPECT_EQ(VEIO, proxy->changeFileGroup("/path", 456));

    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy->changeFileGroup("/path", 456));

    msg.set_gname("groupname");
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy->changeFileGroup("/path", 456, "groupname"));
}
