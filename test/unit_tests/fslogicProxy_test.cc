/**
 * @file fslogicProxy_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "fslogicProxy_proxy.h"
#include "messageBuilder_mock.h"
#include "config_mock.h"
#include "jobScheduler_mock.h"

using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class FslogicProxyTest 
    : public ::testing::Test 
{
protected:
    COMMON_DEFS();
    ProxyFslogicProxy proxy;
    shared_ptr<MockMessageBuilder> msgBuilder;

    virtual void SetUp() {
        COMMON_SETUP();

        EXPECT_CALL(*config, isSet(_)).WillRepeatedly(Return(true));
        EXPECT_CALL(*config, getString(_)).WillRepeatedly(Return(""));
        EXPECT_CALL(*config, getInt(_)).WillRepeatedly(Return(0));
        
        msgBuilder.reset(new MockMessageBuilder());
        proxy.setMessageBuilder(msgBuilder);
        proxy.mockAtom = false;
        proxy.mockSerialized = false;
        proxy.ch_mock.reset(new MockCommunicationHandler());
        EXPECT_CALL(*connectionPool, selectConnection(_, _)).WillRepeatedly(Return(proxy.ch_mock));
        EXPECT_CALL(*connectionPool, releaseConnection(_)).WillRepeatedly(Return());
    }

    virtual void TearDown() {
        COMMON_CLEANUP();
    }

};

TEST_F(FslogicProxyTest, sendFuseReceiveSerializedMessageFails) {
    EXPECT_CALL(*msgBuilder, packFuseMessage("messageType", "answerType", FUSE_MESSAGES, "messageInput")).WillOnce(Return((ClusterMsg*)NULL));
    EXPECT_EQ("", proxy.sendFuseReceiveSerializedMessage("messageType", "answerType", "messageInput"));

    ClusterMsg cMsg;
    EXPECT_CALL(*msgBuilder, packFuseMessage("messageType", "answerType", FUSE_MESSAGES, "messageInput")).WillRepeatedly(Return(&cMsg));

    Answer ans;
    ans.set_answer_status("not ok");
    EXPECT_CALL(*proxy.ch_mock, communicate(Truly(bind(identityEqual<ClusterMsg>, cref(cMsg), _1)), _)).WillOnce(Return(ans));

    EXPECT_EQ("", proxy.sendFuseReceiveSerializedMessage("messageType", "answerType", "messageInput"));
}

TEST_F(FslogicProxyTest, sendFuseReceiveSerializedMessageOK) {
    ClusterMsg cMsg;
    EXPECT_CALL(*msgBuilder, packFuseMessage("messageType", "answerType", FUSE_MESSAGES, "messageInput")).WillRepeatedly(Return(&cMsg));

    Answer ans;
    ans.set_answer_status(VOK);
    ans.set_worker_answer("worker_answer");
    EXPECT_CALL(*proxy.ch_mock, communicate(Truly(bind(identityEqual<ClusterMsg>, cref(cMsg), _1)), _)).WillOnce(Return(ans));

    EXPECT_EQ("worker_answer", proxy.sendFuseReceiveSerializedMessage("messageType", "answerType", "messageInput"));
}

TEST_F(FslogicProxyTest, sendFuseReceiveAtomMessageFails) {
    EXPECT_CALL(*msgBuilder, packFuseMessage("messageType", ATOM, COMMUNICATION_PROTOCOL, "messageInput")).WillOnce(Return((ClusterMsg*)NULL));
    EXPECT_EQ("", proxy.sendFuseReceiveAtomMessage("messageType", "messageInput"));


    ClusterMsg cMsg;
    EXPECT_CALL(*msgBuilder, packFuseMessage("messageType", ATOM, COMMUNICATION_PROTOCOL, "messageInput")).WillRepeatedly(Return(&cMsg));

    Answer ans;
    ans.set_answer_status("not ok");
    ans.set_worker_answer("asnwer");
    EXPECT_CALL(*proxy.ch_mock, communicate(Truly(bind(identityEqual<ClusterMsg>, cref(cMsg), _1)), _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return(""));
    EXPECT_EQ("", proxy.sendFuseReceiveAtomMessage("messageType", "messageInput"));
}

TEST_F(FslogicProxyTest, sendFuseReceiveAtomMessageOK) {
    ClusterMsg cMsg;
    EXPECT_CALL(*msgBuilder, packFuseMessage("messageType", ATOM, COMMUNICATION_PROTOCOL, "messageInput")).WillRepeatedly(Return(&cMsg));

    Answer ans;
    ans.set_answer_status(VOK);
    EXPECT_CALL(*proxy.ch_mock, communicate(Truly(bind(identityEqual<ClusterMsg>, cref(cMsg), _1)), _)).WillOnce(Return(ans));
    EXPECT_CALL(*msgBuilder, decodeAtomAnswer(_)).WillOnce(Return("atom"));

    EXPECT_EQ("atom", proxy.sendFuseReceiveAtomMessage("messageType", "messageInput"));
}

TEST_F(FslogicProxyTest, getFileAttr) {
    proxy.mockSerialized = true;
    GetFileAttr msg;
    msg.set_file_logic_name("/file");

    FileAttr attributes;
    FileAttr response;

    EXPECT_CALL(proxy, mockSerializedFun(GET_FILE_ATTR, FILE_ATTR, msg.SerializeAsString())).WillOnce(Return("wat? error"));
    EXPECT_FALSE(proxy.getFileAttr("/file", &response));

    attributes.set_atime(0);
    attributes.set_mtime(0);
    attributes.set_ctime(0);
    attributes.set_gid(1);
    attributes.set_uid(2);
    attributes.set_mode(1234);
    attributes.set_nlink(3);
    attributes.set_type("type");
    EXPECT_CALL(proxy, mockSerializedFun(GET_FILE_ATTR, FILE_ATTR, msg.SerializeAsString())).WillOnce(Return(attributes.SerializeAsString()));
    ASSERT_TRUE(proxy.getFileAttr("/file", &response));

    EXPECT_EQ(attributes.nlink(), response.nlink());
    EXPECT_EQ(attributes.mode(), response.mode());
    EXPECT_EQ(attributes.type(), response.type());
}

TEST_F(FslogicProxyTest, getFileLocation) {
    proxy.mockSerialized = true;
    GetFileLocation msg;
    msg.set_file_logic_name("/file");

    FileLocation location;
    FileLocation response;

    EXPECT_CALL(proxy, mockSerializedFun(GET_FILE_LOCATION, FILE_LOCATION, msg.SerializeAsString())).WillOnce(Return("wat? error"));
    EXPECT_FALSE(proxy.getFileLocation("/file", &response));

    location.set_validity(10);
    location.set_answer(VEACCES);
    location.set_storage_id(4);
    location.set_file_id("fileid");
    EXPECT_CALL(proxy, mockSerializedFun(GET_FILE_LOCATION, FILE_LOCATION, msg.SerializeAsString())).WillOnce(Return(location.SerializeAsString()));
    ASSERT_TRUE(proxy.getFileLocation("/file", &response));

    EXPECT_EQ(location.validity(), response.validity());
    EXPECT_EQ(location.answer(), response.answer());
}

TEST_F(FslogicProxyTest, getNewFileLocation) {
    proxy.mockSerialized = true;
    GetNewFileLocation msg;
    msg.set_file_logic_name("/file");
    msg.set_mode(234);

    FileLocation location;
    FileLocation response;

    EXPECT_CALL(proxy, mockSerializedFun(GET_NEW_FILE_LOCATION, FILE_LOCATION, msg.SerializeAsString())).WillOnce(Return("wat? error"));
    EXPECT_FALSE(proxy.getNewFileLocation("/file", 234, &response));

    location.set_validity(10);
    location.set_answer(VEACCES);
    location.set_storage_id(4);
    location.set_file_id("fileid");
    EXPECT_CALL(proxy, mockSerializedFun(GET_NEW_FILE_LOCATION, FILE_LOCATION, msg.SerializeAsString())).WillOnce(Return(location.SerializeAsString()));
    ASSERT_TRUE(proxy.getNewFileLocation("/file", 234, &response));

    EXPECT_EQ(location.validity(), response.validity());
    EXPECT_EQ(location.answer(), response.answer());

}

TEST_F(FslogicProxyTest, renewFileLocation) {
    proxy.mockSerialized = true;
    RenewFileLocation msg;
    msg.set_file_logic_name("/file");

    EXPECT_CALL(proxy, mockSerializedFun(RENEW_FILE_LOCATION, FILE_LOCATION_VALIDITY, msg.SerializeAsString())).WillOnce(Return("parse problems"));
    EXPECT_GT(0, proxy.renewFileLocation("/file"));

    FileLocationValidity validity;
    EXPECT_CALL(proxy, mockSerializedFun(RENEW_FILE_LOCATION, FILE_LOCATION_VALIDITY, msg.SerializeAsString())).WillOnce(Return(validity.SerializeAsString()));
    EXPECT_GT(0, proxy.renewFileLocation("/file"));  

    validity.set_answer(VOK);
    EXPECT_CALL(proxy, mockSerializedFun(RENEW_FILE_LOCATION, FILE_LOCATION_VALIDITY, msg.SerializeAsString())).WillOnce(Return(validity.SerializeAsString()));
    EXPECT_GT(0, proxy.renewFileLocation("/file")); 

    validity.set_answer(VOK);
    validity.set_validity(-1);
    EXPECT_CALL(proxy, mockSerializedFun(RENEW_FILE_LOCATION, FILE_LOCATION_VALIDITY, msg.SerializeAsString())).WillOnce(Return(validity.SerializeAsString()));
    EXPECT_GT(0, proxy.renewFileLocation("/file")); 

    validity.set_answer(VEACCES);
    validity.set_validity(10);
    EXPECT_CALL(proxy, mockSerializedFun(RENEW_FILE_LOCATION, FILE_LOCATION_VALIDITY, msg.SerializeAsString())).WillOnce(Return(validity.SerializeAsString()));
    EXPECT_GT(0, proxy.renewFileLocation("/file")); 

    validity.set_answer(VOK);
    validity.set_validity(15);
    EXPECT_CALL(proxy, mockSerializedFun(RENEW_FILE_LOCATION, FILE_LOCATION_VALIDITY, msg.SerializeAsString())).WillOnce(Return(validity.SerializeAsString()));
    EXPECT_EQ(15, proxy.renewFileLocation("/file")); 
}

TEST_F(FslogicProxyTest, getFileChildren) {
    proxy.mockSerialized = true;
    std::vector<std::string> childrenVect;
    GetFileChildren msg;
    msg.set_dir_logic_name("/dir");
    msg.set_children_num(10); 
    msg.set_offset(5);

    FileChildren children;

    EXPECT_CALL(proxy, mockSerializedFun(GET_FILE_CHILDREN, FILE_CHILDREN, msg.SerializeAsString())).WillOnce(Return("something horribly wrong"));
    EXPECT_FALSE(proxy.getFileChildren("/dir", 10, 5, &childrenVect));

    childrenVect.clear();
    FileChildren response;
    EXPECT_CALL(proxy, mockSerializedFun(GET_FILE_CHILDREN, FILE_CHILDREN, msg.SerializeAsString())).WillOnce(Return(response.SerializeAsString()));
    EXPECT_TRUE(proxy.getFileChildren("/dir", 10, 5, &childrenVect));
    EXPECT_EQ(0, childrenVect.size());

    response.add_child_logic_name("/child2");
    response.add_child_logic_name("/child1");
    EXPECT_CALL(proxy, mockSerializedFun(GET_FILE_CHILDREN, FILE_CHILDREN, msg.SerializeAsString())).WillOnce(Return(response.SerializeAsString()));
    EXPECT_TRUE(proxy.getFileChildren("/dir", 10, 5, &childrenVect));
    EXPECT_EQ(2, childrenVect.size());
    EXPECT_EQ("/child2", childrenVect[0]);
    EXPECT_EQ("/child1", childrenVect[1]);
}

TEST_F(FslogicProxyTest, createDir) {
    proxy.mockAtom = true;

    CreateDir msg;
    msg.set_dir_logic_name("/dir");
    msg.set_mode(1234);

    EXPECT_CALL(proxy, mockAtomFun(CREATE_DIR, msg.SerializeAsString())).WillOnce(Return(""));
    EXPECT_EQ(VEIO, proxy.createDir("/dir", 1234));

    EXPECT_CALL(proxy, mockAtomFun(CREATE_DIR, msg.SerializeAsString())).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy.createDir("/dir", 1234));

    EXPECT_CALL(proxy, mockAtomFun(CREATE_DIR, msg.SerializeAsString())).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy.createDir("/dir", 1234));
}

TEST_F(FslogicProxyTest, deleteFile) {
    proxy.mockAtom = true;

    DeleteFile msg;
    msg.set_file_logic_name("/path");

    EXPECT_CALL(proxy, mockAtomFun(DELETE_FILE, msg.SerializeAsString())).WillOnce(Return(""));
    EXPECT_EQ(VEIO, proxy.deleteFile("/path"));

    EXPECT_CALL(proxy, mockAtomFun(DELETE_FILE, msg.SerializeAsString())).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy.deleteFile("/path"));

    EXPECT_CALL(proxy, mockAtomFun(DELETE_FILE, msg.SerializeAsString())).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy.deleteFile("/path"));
}

TEST_F(FslogicProxyTest, sendFileNotUsed) {
    proxy.mockAtom = true;

    FileNotUsed msg;
    msg.set_file_logic_name("/path");

    EXPECT_CALL(proxy, mockAtomFun(FILE_NOT_USED, msg.SerializeAsString())).WillOnce(Return(""));
    EXPECT_FALSE(proxy.sendFileNotUsed("/path"));

    EXPECT_CALL(proxy, mockAtomFun(FILE_NOT_USED, msg.SerializeAsString())).WillOnce(Return(VOK));
    EXPECT_TRUE(proxy.sendFileNotUsed("/path"));

    EXPECT_CALL(proxy, mockAtomFun(FILE_NOT_USED, msg.SerializeAsString())).WillOnce(Return(VEACCES));
    EXPECT_FALSE(proxy.sendFileNotUsed("/path"));
}

TEST_F(FslogicProxyTest, renameFile) {
    proxy.mockAtom = true;

    RenameFile msg;
    msg.set_from_file_logic_name("/path");
    msg.set_to_file_logic_name("/new/path");

    EXPECT_CALL(proxy, mockAtomFun(RENAME_FILE, msg.SerializeAsString())).WillOnce(Return(""));
    EXPECT_EQ(VEIO, proxy.renameFile("/path", "/new/path"));

    EXPECT_CALL(proxy, mockAtomFun(RENAME_FILE, msg.SerializeAsString())).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy.renameFile("/path", "/new/path"));

    EXPECT_CALL(proxy, mockAtomFun(RENAME_FILE, msg.SerializeAsString())).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy.renameFile("/path", "/new/path"));
}

TEST_F(FslogicProxyTest, changeFilePerms) {
    proxy.mockAtom = true;

    ChangeFilePerms msg;
    msg.set_logic_file_name("/path");
    msg.set_perms(123);

    EXPECT_CALL(proxy, mockAtomFun(CHANGE_FILE_PERMS, msg.SerializeAsString())).WillOnce(Return(""));
    EXPECT_EQ(VEIO, proxy.changeFilePerms("/path", 123));

    EXPECT_CALL(proxy, mockAtomFun(CHANGE_FILE_PERMS, msg.SerializeAsString())).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy.changeFilePerms("/path", 123));

    EXPECT_CALL(proxy, mockAtomFun(CHANGE_FILE_PERMS, msg.SerializeAsString())).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy.changeFilePerms("/path", 123));

}

TEST_F(FslogicProxyTest, createLink) {
    proxy.mockAtom = true;

    CreateLink msg;
    msg.set_from_file_logic_name("/from");
    msg.set_to_file_logic_name("/to");

    EXPECT_CALL(proxy, mockAtomFun(CREATE_LINK, msg.SerializeAsString())).WillOnce(Return(""));
    EXPECT_EQ(VEIO, proxy.createLink("/from", "/to"));

    EXPECT_CALL(proxy, mockAtomFun(CREATE_LINK, msg.SerializeAsString())).WillOnce(Return(VOK));
    EXPECT_EQ(VOK, proxy.createLink("/from", "/to"));

    EXPECT_CALL(proxy, mockAtomFun(CREATE_LINK, msg.SerializeAsString())).WillOnce(Return(VEACCES));
    EXPECT_EQ(VEACCES, proxy.createLink("/from", "/to"));
}

TEST_F(FslogicProxyTest, getLink) {
    proxy.mockSerialized = true;

    GetLink msg;
    msg.set_file_logic_name("/from");

    LinkInfo response;

    pair<string, string> resp;

    EXPECT_CALL(proxy, mockSerializedFun(GET_LINK, LINK_INFO, msg.SerializeAsString())).WillOnce(Return(""));
    resp = proxy.getLink("/from");
    EXPECT_EQ(VEIO, resp.first);

    response.set_file_logic_name("/to");
    EXPECT_CALL(proxy, mockSerializedFun(GET_LINK, LINK_INFO, msg.SerializeAsString())).WillOnce(Return(response.SerializeAsString()));
    resp = proxy.getLink("/from");
    EXPECT_EQ(VOK, resp.first);
    EXPECT_EQ("/to", resp.second);

    response.set_answer(VEACCES);
    response.set_file_logic_name("");
    EXPECT_CALL(proxy, mockSerializedFun(GET_LINK, LINK_INFO, msg.SerializeAsString())).WillOnce(Return(response.SerializeAsString()));
    resp = proxy.getLink("/from");
    EXPECT_EQ(VEACCES, resp.first);
}