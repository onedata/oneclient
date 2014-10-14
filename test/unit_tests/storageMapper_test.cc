/**
 * @file storageMapper_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication_protocol.pb.h"
#include "fslogicProxy_mock.h"
#include "helpers/storageHelperFactory.h"
#include "options_mock.h"
#include "storageMapper_proxy.h"
#include "scheduler_mock.h"
#include "testCommon.h"
#include "fsImpl.h"
#include "fuse_messages.pb.h"
#include "oneException.h"

using namespace ::testing;
using namespace one;
using namespace one::client;
using namespace one::clproto::fuse_messages;
using namespace one::clproto::communication_protocol;
using namespace std::literals::chrono_literals;

class StorageMapperTest: public CommonTest
{
protected:
    std::shared_ptr<MockFslogicProxy> mockFslogic;
    std::shared_ptr<ProxyStorageMapper> proxy;

    void SetUp() override
    {
        CommonTest::SetUp();
        mockFslogic = std::make_shared<MockFslogicProxy>(context);
        proxy = std::make_shared<ProxyStorageMapper>(context, mockFslogic);
        ON_CALL(*scheduler, schedule(_, _)).WillByDefault(Return([]{}));
    }
};

TEST_F(StorageMapperTest, AddAndGet) {
    EXPECT_EQ(0u, proxy->getStorageMapping().size());
    EXPECT_EQ(0u, proxy->getFileMappingSize());

    FileLocation location;
    location.set_validity(10);
    location.set_storage_id(1);
    location.add_storage_helper_args("arg0");
    location.add_storage_helper_args("arg1");

    EXPECT_THROW(proxy->getLocationInfo("/file1"), OneException);
    EXPECT_CALL(*scheduler, schedule(_, _)).Times(2);
    proxy->addLocation("/file1", location);
    EXPECT_EQ(1u, proxy->getStorageMapping().size());
    EXPECT_EQ(1u, proxy->getFileMappingSize());

    EXPECT_CALL(*scheduler, schedule(_, _)).Times(2);
    proxy->addLocation("/file1", location);
    EXPECT_EQ(1u, proxy->getStorageMapping().size());
    EXPECT_EQ(1u, proxy->getFileMappingSize());

    EXPECT_THROW(proxy->getLocationInfo("/file0"), OneException);
    EXPECT_NO_THROW(proxy->getLocationInfo("/file1"));

    location.set_validity(20);
    location.set_storage_id(2);
    location.clear_storage_helper_args();
    location.add_storage_helper_args("arg2");
    location.add_storage_helper_args("arg3");
    EXPECT_CALL(*scheduler, schedule(_, _)).Times(2);
    proxy->addLocation("/file2", location);
    EXPECT_NO_THROW(proxy->getLocationInfo("/file2"));

    std::pair<LocationInfo, StorageInfo> ret1 = proxy->getLocationInfo("/file1");
    std::pair<LocationInfo, StorageInfo> ret2 = proxy->getLocationInfo("/file2");
    EXPECT_EQ(1, ret1.first.storageId);
    EXPECT_EQ(2, ret2.first.storageId);

    EXPECT_EQ("arg0", boost::any_cast<std::string>(ret1.second.storageHelperArgs[helpers::srvArg(0)]));
    EXPECT_EQ("arg3", boost::any_cast<std::string>(ret2.second.storageHelperArgs[helpers::srvArg(1)]));
}

TEST_F(StorageMapperTest, OpenClose) {
    EXPECT_CALL(*scheduler, schedule(_, _)).Times(4);
    EXPECT_CALL(*mockFslogic, sendFileNotUsed("/file1")).WillOnce(Return(true));
    EXPECT_CALL(*mockFslogic, sendFileNotUsed("/file2")).WillOnce(Return(true));

    FileLocation location;
    proxy->addLocation("/file1", location);
    location.set_file_id("location");
    proxy->addLocation("/file2", location);

    std::pair<LocationInfo, StorageInfo> ret1 = proxy->getLocationInfo("/file1");
    std::pair<LocationInfo, StorageInfo> ret2 = proxy->getLocationInfo("/file2");

    EXPECT_EQ(0, ret1.first.opened);
    EXPECT_EQ(0, ret2.first.opened);

    proxy->openFile("/file3");
    proxy->openFile("/file1");
    proxy->openFile("/file1");

    EXPECT_EQ(2, proxy->getLocationInfo("/file1").first.opened);
    EXPECT_EQ(0, proxy->getLocationInfo("/file2").first.opened);


    proxy->releaseFile("/file3");
    proxy->releaseFile("/file1");
    proxy->openFile("/file2");
    EXPECT_EQ(1, proxy->getLocationInfo("/file1").first.opened);
    EXPECT_EQ(1, proxy->getLocationInfo("/file2").first.opened);

    proxy->releaseFile("/file1");
    proxy->releaseFile("/file1");
    proxy->releaseFile("/file2");

    EXPECT_THROW(proxy->getLocationInfo("/file1"), OneException);
    EXPECT_THROW(proxy->getLocationInfo("/file2"), OneException);
}

TEST_F(StorageMapperTest, FindAndGet) {
    EXPECT_CALL(*mockFslogic, getFileLocation("/file1", _, _, _)).WillOnce(Return(false));
    EXPECT_NE(VOK, proxy->findLocation("/file1"));

    FileLocation location;
    location.set_answer(VEACCES);
    EXPECT_CALL(*mockFslogic, getFileLocation("/file1", _, _, _)).WillOnce(DoAll(SetArgReferee<1>(location), Return(true)));
    EXPECT_EQ(VEACCES, proxy->findLocation("/file1"));

    EXPECT_THROW(proxy->getLocationInfo("/file1"), OneException);
    location.set_answer(VOK);
    location.set_validity(20);

    EXPECT_CALL(*scheduler, schedule(_, _)).Times(2);
    EXPECT_CALL(*mockFslogic, getFileLocation("/file1", _, _, _)).WillOnce(DoAll(SetArgReferee<1>(location), Return(true)));
    EXPECT_EQ(VOK, proxy->findLocation("/file1"));

    EXPECT_NO_THROW(proxy->getLocationInfo("/file1"));
    EXPECT_THROW(proxy->getLocationInfo("/file2"), OneException);

    auto currentTime = std::chrono::steady_clock::now();
    EXPECT_TRUE(currentTime + 20s >= proxy->getLocationInfo("/file1").first.validTo);

    EXPECT_CALL(*mockFslogic, getFileLocation("/file2", _, _, _)).WillOnce(Return(false));
    EXPECT_THROW(proxy->getLocationInfo("/file2", true), OneException);

    location.set_file_id("other");
    EXPECT_CALL(*scheduler, schedule(_, _)).Times(2);
    EXPECT_CALL(*mockFslogic, getFileLocation("/file2", _, _, _)).WillOnce(DoAll(SetArgReferee<1>(location), Return(true)));
    EXPECT_NO_THROW(proxy->getLocationInfo("/file2", true));
}

TEST_F(StorageMapperTest, ShouldHandleBlocksAvailableMessages) {
    FileLocation location;
    location.set_answer(VOK);
    location.set_validity(20);
    location.set_storage_id(1);
    location.set_file_id("123");
    EXPECT_CALL(*mockFslogic, getFileLocation("/file", _, _, _)).WillOnce(DoAll(SetArgReferee<1>(location), Return(true)));

    const auto info = proxy->getLocationInfo("/file", true);
    ASSERT_TRUE(info.first.blocks.empty());

    FileLocation::BlockAvailability block;
    block.set_offset(0);
    block.set_size(100);

    BlocksAvailable msg;
    msg.add_blocks()->CopyFrom(block);
    msg.set_storage_id(1);
    msg.set_file_id("123");

    Answer ans;
    ans.set_answer_status(VOK);
    ans.set_message_type(msg.descriptor()->name());
    msg.SerializeToString(ans.mutable_worker_answer());

    proxy->handlePushMessage(ans);

    const auto updatedInfo = proxy->getLocationInfo("/file", true);
    ASSERT_EQ(100u, updatedInfo.first.blocks.size());
    ASSERT_NE(updatedInfo.first.blocks.end(),
              updatedInfo.first.blocks.find(boost::icl::discrete_interval<off_t>::right_open(0, 100)));
}

TEST_F(StorageMapperTest, ShouldWaitForABlock) {
    FileLocation location;
    location.set_answer(VOK);
    location.set_validity(20);
    location.set_storage_id(1);
    location.set_file_id("123");
    EXPECT_CALL(*mockFslogic, getFileLocation("/file", _, _, _)).WillOnce(DoAll(SetArgReferee<1>(location), Return(true)));

    proxy->getLocationInfo("/file", true);

    std::thread t{[=]{
        ASSERT_TRUE(proxy->waitForBlock("/file", 0));
    }};

    FileLocation::BlockAvailability block;
    block.set_offset(0);
    block.set_size(100);

    BlocksAvailable msg;
    msg.add_blocks()->CopyFrom(block);
    msg.set_storage_id(1);
    msg.set_file_id("123");

    Answer ans;
    ans.set_answer_status(VOK);
    ans.set_message_type(msg.descriptor()->name());
    msg.SerializeToString(ans.mutable_worker_answer());

    proxy->handlePushMessage(ans);

    t.join();
}

TEST_F(StorageMapperTest, ShouldTimeoutOnWaitingForABlockIfNoBlockArrives) {
    FileLocation location;
    location.set_answer(VOK);
    location.set_validity(20);
    location.set_storage_id(1);
    location.set_file_id("123");
    EXPECT_CALL(*mockFslogic, getFileLocation("/file", _, _, _)).WillOnce(DoAll(SetArgReferee<1>(location), Return(true)));

    proxy->getLocationInfo("/file", true);

    const auto then = std::chrono::steady_clock::now();
    ASSERT_FALSE(proxy->waitForBlock("/file", 0, 20ms));
    ASSERT_TRUE(then + 20ms <= std::chrono::steady_clock::now());
}

TEST_F(StorageMapperTest, ShouldSetBlocksAsAvailableOnAddFileMapping) {
    FileLocation::BlockAvailability block;
    block.set_offset(0);
    block.set_size(100);

    FileLocation location;
    location.set_answer(VOK);
    location.set_validity(20);
    location.set_storage_id(1);
    location.set_file_id("123");
    location.add_available()->CopyFrom(block);

    EXPECT_CALL(*mockFslogic, getFileLocation("/file", _, _, _)).WillOnce(DoAll(SetArgReferee<1>(location), Return(true)));

    const auto info = proxy->getLocationInfo("/file", true);
    ASSERT_EQ(100u, info.first.blocks.size());
    ASSERT_NE(info.first.blocks.end(),
              info.first.blocks.find(boost::icl::discrete_interval<off_t>::right_open(0, 100)));
}
