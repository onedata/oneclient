/**
 * @file fsImpl_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "communication/communicator_mock.h"
#include "config.h"
#include "events/events.h"
#include "events_mock.h"
#include "fslogicProxy_mock.h"
#include "localStorageManager_mock.h"
#include "messageBuilder_mock.h"
#include "metaCache_mock.h"
#include "options_mock.h"
#include "scheduler_mock.h"
#include "storageHelperFactory_fake.h"
#include "storageMapper_mock.h"
#include "testCommon.h"
#include "oneErrors.h"
#include "fsImpl_proxy.h"
#include "oneException.h"

using namespace ::testing;
using namespace std::placeholders;
using namespace one::client;
using namespace one::client::events;
using namespace one::clproto::fuse_messages;

template<typename T> bool identityEqual( const T &lhs, const T &rhs ) { return &lhs == &rhs; }

int getError(std::function<int()> f)
{
    try {
        return f();
    }
    catch (OneException e) {
        return one::translateError(e.oneError());
    }
}

class FsImplTest: public CommonTest
{
public:
    std::shared_ptr<ProxyFsImpl> client;
    std::shared_ptr<MockFslogicProxy> fslogicMock;
    std::shared_ptr<MockMetaCache> metaCacheMock;
    std::shared_ptr<MockLocalStorageManager> storageManagerMock;
    std::shared_ptr<MockStorageMapper> storageMapperMock;
    std::shared_ptr<MockGenericHelper> helperMock;
    std::shared_ptr<FakeStorageHelperFactory> factoryFake;
    std::shared_ptr<MockEventCommunicator> eventCommunicatorMock;

    struct fuse_file_info fileInfo;
    struct stat trueStat;
    FileAttr trueAttr;
    LocationInfo location;
    StorageInfo storage;

    void SetUp() override
    {
        context = std::make_shared<Context>();

        options = std::make_shared<MockOptions>();
        context->setOptions(options);
        config = std::make_shared<one::client::Config>(context);
        context->setConfig(config);
        scheduler = std::make_shared<MockScheduler>();
        context->setScheduler(scheduler);
        communicator = std::make_shared<MockCommunicator>();
        context->setCommunicator(communicator);
        fslogicMock = std::make_shared<MockFslogicProxy>(context);
        storageMapperMock = std::make_shared<MockStorageMapper>(context, fslogicMock);
        context->setStorageMapper(storageMapperMock);

        metaCacheMock = std::make_shared<MockMetaCache>(context);
        storageManagerMock = std::make_shared<MockLocalStorageManager>(context);
        helperMock = std::make_shared<MockGenericHelper>();
        factoryFake = std::make_shared<FakeStorageHelperFactory>();
        eventCommunicatorMock = std::make_shared<MockEventCommunicator>(context);

        EXPECT_CALL(*fslogicMock, pingCluster()).WillRepeatedly(Return());
        EXPECT_CALL(*options, get_alive_meta_connections_count()).WillRepeatedly(Return(0));
        EXPECT_CALL(*options, get_alive_data_connections_count()).WillRepeatedly(Return(0));
        EXPECT_CALL(*options, has_fuse_id()).WillRepeatedly(Return(false));
        EXPECT_CALL(*options, has_fuse_group_id()).WillRepeatedly(Return(true));
        EXPECT_CALL(*options, get_write_bytes_before_stat()).WillRepeatedly(Return(0));
        ON_CALL(*scheduler, schedule(_, _)).WillByDefault(Return([]{}));

        const ::testing::TestInfo* const test_info = ::testing::UnitTest::GetInstance()->current_test_info();
        std::string testCaseName = test_info->test_case_name();
        if(testCaseName == "writeDisabled"){
            EXPECT_CALL(*fslogicMock, isWriteEnabled()).WillRepeatedly(Return(false));
        }else{
            EXPECT_CALL(*fslogicMock, isWriteEnabled()).WillRepeatedly(Return(true));
        }

        client = std::make_shared<ProxyFsImpl>("/root", context,
                        fslogicMock,
                        metaCacheMock,
                        storageManagerMock,
                        factoryFake,
                        eventCommunicatorMock);

        factoryFake->presetMock = helperMock;

        location.fileId = "fileid";
        location.storageId = 1;
        location.blocks += boost::icl::discrete_interval<off_t>::closed(0, 100);
        storage.storageHelperName = "sh_name";
        storage.storageHelperArgs.emplace(one::helpers::srvArg(0), boost::any{std::string{"arg1"}});
        storage.storageHelperArgs.emplace(one::helpers::srvArg(1), boost::any{std::string{"arg2"}});

        trueStat.st_atime = 1;
        trueStat.st_ctime = 2;
        trueStat.st_mtime = 3;
        trueStat.st_mode = 4;
        trueStat.st_gid = 5;
        trueStat.st_uid = 6;
        trueStat.st_size = 7;

        trueAttr.set_atime(8);
        trueAttr.set_ctime(9);
        trueAttr.set_mtime(10);
        trueAttr.set_mode(11);
        trueAttr.set_gid(54321);
        trueAttr.set_uid(0);

        fileInfo.fh = 0;
        client->setCachedHelper(0, helperMock);
    }
};

TEST_F(FsImplTest, Instantiate) {
    EXPECT_EQ(scheduler.get(), context->scheduler().get());
    EXPECT_TRUE(context->getPushListener().get());
}

TEST_F(FsImplTest, translateError) {
    EXPECT_EQ(0, one::translateError(VOK));
    EXPECT_EQ(-ENOENT, one::translateError(VENOENT));
    EXPECT_EQ(-EACCES, one::translateError(VEACCES));
    EXPECT_EQ(-EEXIST, one::translateError(VEEXIST));
    EXPECT_EQ(-EIO, one::translateError(VEIO));

    EXPECT_EQ(-EIO, one::translateError("unknown"));
    EXPECT_EQ(-EIO, one::translateError("other unknown"));
}

TEST_F(FsImplTest, access) { // const char *path, int mask
    EXPECT_EQ(0, client->access("/path", 1234));
}

TEST_F(FsImplTest, getattrCache) { // const char *path, struct stat *statbuf
    struct stat statbuf;

    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(DoAll(SetArgPointee<1>(trueStat), Return(true)));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));
    EXPECT_EQ(trueStat.st_atime, statbuf.st_atime);
    EXPECT_EQ(trueStat.st_ctime, statbuf.st_ctime);
    EXPECT_EQ(trueStat.st_mtime, statbuf.st_mtime);
    EXPECT_EQ(trueStat.st_mode, statbuf.st_mode);
    EXPECT_EQ(trueStat.st_gid, statbuf.st_gid);
    EXPECT_EQ(trueStat.st_uid, statbuf.st_uid);
    EXPECT_EQ(trueStat.st_size, statbuf.st_size);
}

TEST_F(FsImplTest, getattrNoCluster) { // const char *path, struct stat *statbuf
    struct stat statbuf;

    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(Return(false));
    EXPECT_EQ(-EIO, client->getattr("/path", &statbuf));
}

TEST_F(FsImplTest, getattr) { // const char *path, struct stat *statbuf
    struct stat statbuf;

    EXPECT_CALL(*options, get_enable_dir_prefetch()).WillOnce(Return(true));
    EXPECT_CALL(*options, get_enable_attr_cache()).WillOnce(Return(true));
    EXPECT_CALL(*scheduler, post(_));

    trueAttr.set_type("DIR");
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(Return(false));
    EXPECT_EQ(-EIO, client->getattr("/path", &statbuf));

    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgReferee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*metaCacheMock, addAttr(_, "/path", Truly(bind(identityEqual<struct stat>, std::cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));

    EXPECT_EQ(trueAttr.atime(), statbuf.st_atime);
    EXPECT_EQ(trueAttr.ctime(), statbuf.st_ctime);
    EXPECT_EQ(trueAttr.mtime(), statbuf.st_mtime);

    EXPECT_EQ(trueAttr.size(), statbuf.st_size);
    EXPECT_EQ(static_cast<gid_t>(trueAttr.gid()), statbuf.st_gid);
    EXPECT_EQ(0u, statbuf.st_uid); // Its root

    EXPECT_EQ(static_cast<mode_t>(trueAttr.mode()) | S_IFDIR, statbuf.st_mode);

    trueAttr.set_type("LNK");
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgReferee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*metaCacheMock, addAttr(_, "/path", Truly(bind(identityEqual<struct stat>, std::cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));

    EXPECT_EQ(static_cast<mode_t>(trueAttr.mode()) | S_IFLNK, statbuf.st_mode);

    trueAttr.set_type("REG");
    EXPECT_CALL(*scheduler, post(_));
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgReferee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*metaCacheMock, addAttr(_, "/path", Truly(bind(identityEqual<struct stat>, std::cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));

    EXPECT_EQ(static_cast<mode_t>(trueAttr.mode()) | S_IFREG, statbuf.st_mode);
}

TEST_F(FsImplTest, readlink) { // const char *path, char *link, size_t size
    char link[5];

    EXPECT_CALL(*fslogicMock, getLink("/path")).WillOnce(Return(std::make_pair(VENOENT, "")));
    EXPECT_EQ(-ENOENT, client->readlink("/path", link, 5));

    EXPECT_CALL(*fslogicMock, getLink("/path1")).WillOnce(Return(std::make_pair(VOK, "1234")));
    EXPECT_EQ(0, client->readlink("/path1", link, 5));
    EXPECT_EQ("1234", std::string(link));

    EXPECT_CALL(*fslogicMock, getLink("/path2")).WillOnce(Return(std::make_pair(VOK, "12345")));
    EXPECT_EQ(0, client->readlink("/path2", link, 5));
    EXPECT_EQ("1234", std::string(link));

    EXPECT_CALL(*fslogicMock, getLink("/path3")).WillOnce(Return(std::make_pair(VOK, "123456")));
    EXPECT_EQ(0, client->readlink("/path3", link, 5));
    EXPECT_EQ("1234", std::string(link));

    EXPECT_CALL(*fslogicMock, getLink("/path4")).WillOnce(Return(std::make_pair(VOK, "/1234")));
    EXPECT_EQ(0, client->readlink("/path4", link, 5));
    EXPECT_EQ("/roo", std::string(link));
}

TEST_F(FsImplTest, mknod) { // const char *path, mode_t mode, dev_t dev
    FileLocation newLoc;
    dev_t dev = 0;
    EXPECT_CALL(*metaCacheMock, clearAttr("/path")).Times(AtLeast(3));

    newLoc.set_answer(VOK);
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _, _)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck(_)).Times(0);
    EXPECT_EQ(-EIO, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VEACCES);
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck(_)).Times(0);
    EXPECT_EQ(-EACCES, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return());
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillOnce(Throw(OneException(VEACCES)));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck(_)).Times(0);
    EXPECT_EQ(-EACCES, getError([&]{ return client->mknod("/path", 123 | S_IFREG, dev); }));

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return());
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillOnce(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_mknod(StrEq("fileid"), 123 | S_IFREG, dev));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck("/path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VEEXIST);
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck(_)).Times(0);
    EXPECT_EQ(-EEXIST, getError([&]{ return client->mknod("/path", 123 | S_IFREG, dev); }));

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return());
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillOnce(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_mknod(StrEq("fileid"), 123 | S_IFREG, dev)).WillOnce(Return(-EEXIST));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck("/path")).WillOnce(Return(VEIO));
    EXPECT_EQ(-EIO, getError([&]{ return client->mknod("/path", 123 | S_IFREG, dev); }));

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return());
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillOnce(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_mknod(StrEq("fileid"), 123 | S_IFREG, dev)).WillOnce(Return(-EEXIST));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck("/path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->mknod("/path", 123 | S_IFREG, dev));

}

TEST_F(FsImplTest, mkdir) { // const char *path, mode_t mode
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*metaCacheMock, clearAttr("/")).WillRepeatedly(Return());
    EXPECT_CALL(*fslogicMock, createDir("/path", 123)).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->mkdir("/path", 123 | S_IFDIR));

    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*fslogicMock, createDir("/path", 123)).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->mkdir("/path", 123 | S_IFDIR));
}

TEST_F(FsImplTest, unlink) { // const char *path
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillRepeatedly(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path")).Times(AtLeast(3));

    struct stat st = {0};
    st.st_mode |= S_IFLNK;
    FileAttr attrs;
    attrs.set_type("LNK");

    struct stat root_st = {0};
    root_st.st_mode |= S_IFDIR;
    EXPECT_CALL(*metaCacheMock, getAttr("/", _)).WillRepeatedly(DoAll(SetArgPointee<1>(root_st), Return(true)));

    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(st), Return(true)));
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*storageMapperMock, getLocationInfo(_, _, _)).Times(0);
    EXPECT_CALL(*helperMock, sh_unlink(_)).Times(0);
    EXPECT_EQ(0, client->unlink("/path"));

    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(st), Return(false)));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgReferee<1>(attrs), Return(true)));
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*storageMapperMock, getLocationInfo(_, _, _)).Times(0);
    EXPECT_CALL(*helperMock, sh_unlink(_)).Times(0);
    EXPECT_EQ(0, client->unlink("/path"));

    attrs.set_type("REG");
    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillRepeatedly(DoAll(SetArgPointee<1>(st), Return(false)));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillRepeatedly(DoAll(SetArgReferee<1>(attrs), Return(true)));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillRepeatedly(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->unlink("/path"));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillRepeatedly(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VENOENT));
    EXPECT_EQ(-ENOENT, client->unlink("/path"));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillOnce(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_unlink(StrEq("fileid"))).WillOnce(Return(0));
    EXPECT_EQ(0, client->unlink("/path"));
}

TEST_F(FsImplTest, rmdir) { // const char *path
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*metaCacheMock, clearAttr("/")).WillRepeatedly(Return());
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->rmdir("/path"));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_EQ(0, client->rmdir("/path"));
}

TEST_F(FsImplTest, symlink) { // const char *path, const char *link
    EXPECT_CALL(*fslogicMock, createLink("/link", "/path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->symlink("/path", "/link"));

    EXPECT_CALL(*fslogicMock, createLink("/link", "path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->symlink("path", "/link"));

    EXPECT_CALL(*fslogicMock, createLink("/link", "/path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->symlink("/root/path", "/link"));

    EXPECT_CALL(*fslogicMock, createLink("/link", "/path")).WillOnce(Return(VENOENT));
    EXPECT_EQ(-ENOENT, client->symlink("/path", "/link"));
}

TEST_F(FsImplTest, rename) { // const char *path, const char *newpath
    EXPECT_CALL(*fslogicMock, renameFile("/path", "/new/path")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->rename("/path", "/new/path"));

    EXPECT_CALL(*fslogicMock, renameFile("/path", "/new/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_EQ(0, client->rename("/path", "/new/path"));
}

TEST_F(FsImplTest, link) { // const char *path, const char *newpath
    EXPECT_EQ(-ENOTSUP, client->link("/path", "/link"));
}

TEST_F(FsImplTest, chmod) { // const char *path, mode_t mode
    mode_t dirMode = (123 | S_IFDIR);
    mode_t regMode = (123 | S_IFREG);
    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VENOENT));
    EXPECT_EQ(-ENOENT, getError([&]{ return client->chmod("/path", regMode); }));

    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_EQ(0, client->chmod("/path", dirMode));

    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillOnce(Throw(OneException(VEACCES)));
    EXPECT_EQ(-EACCES, getError([&]{ return client->chmod("/path", regMode); }));

    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillRepeatedly(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_chmod(StrEq("fileid"), regMode)).WillOnce(Return(-EACCES));
    EXPECT_EQ(-EACCES, getError([&]{ return client->chmod("/path", regMode); }));

    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillRepeatedly(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_chmod(StrEq("fileid"), regMode)).WillOnce(Return(0));
    EXPECT_EQ(0, client->chmod("/path", regMode));
}

TEST_F(FsImplTest, chown) { // const char *path, uid_t uid, gid_t gid

    #ifdef __APPLE__
        std::string group = "wheel";
    #else
        std::string group = "root";
    #endif

    EXPECT_CALL(*metaCacheMock, clearAttr("/path")).WillRepeatedly(Return());

    EXPECT_EQ(0, client->chown("/path", -1,-1)); // Dont change perms

    EXPECT_CALL(*fslogicMock, changeFileGroup(_, _, _)).Times(0);
    EXPECT_CALL(*fslogicMock, changeFileOwner("/path", 0, "root")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, getError([&]{ return client->chown("/path", 0, -1); }));

    EXPECT_CALL(*fslogicMock, changeFileOwner("/path", 0, "root")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", 0, -1));

    EXPECT_CALL(*fslogicMock, changeFileOwner("/path", 64231, "")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", 64231, -1));

    EXPECT_CALL(*fslogicMock, changeFileOwner(_, _, _)).Times(0);
    EXPECT_CALL(*fslogicMock, changeFileGroup("/path", 0, group)).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, getError([&]{ return client->chown("/path", -1, 0); }));

    EXPECT_CALL(*fslogicMock, changeFileGroup("/path", 0, group)).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", -1, 0));

    EXPECT_CALL(*fslogicMock, changeFileGroup("/path", 54321, "")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", -1, 54321));

    EXPECT_CALL(*fslogicMock, changeFileOwner("/path", 0, "root")).WillOnce(Return(VOK));
    EXPECT_CALL(*fslogicMock, changeFileGroup("/path", 54321, "")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", 0, 54321));

}

TEST_F(FsImplTest, truncate) { // const char *path, off_t newSize

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillOnce(Throw(OneException(VEACCES)));
    EXPECT_EQ(-EACCES, getError([&]{ return client->truncate("/path", 10); }));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillRepeatedly(Return(std::make_pair(location, storage)));

    EXPECT_CALL(*helperMock, sh_truncate(StrEq("fileid"), _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, getError([&]{ return client->truncate("/path", 10); }));

    EXPECT_CALL(*metaCacheMock, updateSize("/path", 10)).WillOnce(Return(true));

    EXPECT_CALL(*helperMock, sh_truncate(StrEq("fileid"), _)).WillOnce(Return(0));
    EXPECT_EQ(0, client->truncate("/path", 10));
}

TEST_F(FsImplTest, utime) { // const char *path, struct utimbuf *ubuf
    struct utimbuf ubuf;

    EXPECT_CALL(*scheduler, post(_));

    EXPECT_EQ(0, client->utime("/path", &ubuf));
}

TEST_F(FsImplTest, open) { // const char *path, struct fuse_file_info *fileInfo

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillOnce(Throw(OneException(VEACCES)));
    EXPECT_EQ(-EACCES, getError([&]{ return client->open("/path", &fileInfo); }));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true, _)).WillRepeatedly(Return(std::make_pair(location, storage)));

    EXPECT_CALL(*helperMock, sh_open(StrEq("fileid"), _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, getError([&]{ return client->open("/path", &fileInfo); }));

    EXPECT_CALL(*helperMock, sh_open(StrEq("fileid"), _)).WillOnce(Return(0));
    fileInfo.flags |= O_RDWR;
    EXPECT_EQ(0, client->open("/path", &fileInfo));
}

TEST_F(FsImplTest, read) { // const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo
    char tmpBuff[4];

    struct stat statbuf = {0};
    statbuf.st_size = 10;
    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillRepeatedly(DoAll(SetArgPointee<1>(statbuf), Return(true)));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillOnce(Throw(OneException(VEACCES)));
    EXPECT_EQ(-EACCES, getError([&]{ return client->read("/path", tmpBuff, 4, 0, &fileInfo); }));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillRepeatedly(Return(std::make_pair(location, storage)));

    EXPECT_CALL(*helperMock, sh_read(StrEq("fileid"), tmpBuff, 4, 0, _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, getError([&]{ return client->read("/path", tmpBuff, 4, 0, &fileInfo); }));

    EXPECT_CALL(*helperMock, sh_read(StrEq("fileid"), tmpBuff, 4, 0, _)).WillOnce(Return(0));
    EXPECT_EQ(0, client->read("/path", tmpBuff, 4, 0, &fileInfo));
}

TEST_F(FsImplTest, write) { // const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillOnce(Throw(OneException(VEACCES)));
    EXPECT_EQ(-EACCES, getError([&]{ return client->write("/path", "abcd", 4, 0, &fileInfo); }));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillRepeatedly(Return(std::make_pair(location, storage)));

    EXPECT_CALL(*helperMock, sh_write(StrEq("fileid"), StrEq("abcd"), 4, 0, _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, getError([&]{ return client->write("/path", "abcd", 4, 0, &fileInfo); }));

    // Assert that cache is updated correctly
    trueStat.st_size = 2;
    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillRepeatedly(DoAll(SetArgPointee<1>(trueStat), Return(true)));
    EXPECT_CALL(*metaCacheMock, updateSize("/path", 6)).WillOnce(Return(true));

    EXPECT_CALL(*helperMock, sh_write(StrEq("fileid"), StrEq("abcd"), 4, 2, _)).WillOnce(Return(4));
    EXPECT_EQ(4, client->write("/path", "abcd", 4, 2, &fileInfo));

    trueStat.st_size = 7;
    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillRepeatedly(DoAll(SetArgPointee<1>(trueStat), Return(true)));
    EXPECT_CALL(*metaCacheMock, updateSize("/path", _)).Times(0);

    EXPECT_CALL(*helperMock, sh_write(StrEq("fileid"), StrEq("abcd"), 4, 2, _)).WillOnce(Return(4));
    EXPECT_CALL(*eventCommunicatorMock, processEvent(_));
    EXPECT_EQ(4, client->write("/path", "abcd", 4, 2, &fileInfo));

}

TEST_F(FsImplTest, statfs) { // const char *path, struct statvfs *statInfo
    struct statvfs statInfo;
    struct statvfs statFS;

    statFS.f_bsize     = 4096;
    statFS.f_frsize    = 4096;
    statFS.f_blocks    = 4096;     /* size of fs in f_frsize units */
    statFS.f_bfree     = 2048;     /* # free blocks */
    statFS.f_bavail    = 2048;     /* # free blocks for unprivileged users */
    statFS.f_files     = 10000;    /* # inodes */
    statFS.f_ffree     = 10000;    /* # free inodes */
    statFS.f_favail    = 10000;    /* # free inodes for unprivileged users */
    statFS.f_fsid      = 0;        /* file system ID */
    statFS.f_flag      = 0;
    statFS.f_namemax   = NAME_MAX;

    EXPECT_CALL(*fslogicMock, getStatFS()).WillOnce(Return(std::make_pair(VEREMOTEIO, statFS)));
    #ifdef __gnu_linux__
        EXPECT_EQ(-EREMOTEIO, client->statfs("/path", &statInfo));
    #else
        EXPECT_EQ(-EIO, client->statfs("/path", &statInfo));
    #endif

    EXPECT_CALL(*fslogicMock, getStatFS()).WillOnce(Return(std::make_pair(VOK, statFS)));
    EXPECT_EQ(0, client->statfs("/path", &statInfo));

    EXPECT_EQ(statFS.f_bsize,   statInfo.f_bsize);
    EXPECT_EQ(statFS.f_frsize,  statInfo.f_frsize);
    EXPECT_EQ(statFS.f_blocks,  statInfo.f_blocks);
    EXPECT_EQ(statFS.f_bfree,   statInfo.f_bfree);
    EXPECT_EQ(statFS.f_bavail,  statInfo.f_bavail);
    EXPECT_EQ(statFS.f_files,   statInfo.f_files);
    EXPECT_EQ(statFS.f_ffree,   statInfo.f_ffree);
    EXPECT_EQ(statFS.f_favail,  statInfo.f_favail);
    EXPECT_EQ(statFS.f_fsid,    statInfo.f_fsid);
    EXPECT_EQ(statFS.f_flag,    statInfo.f_flag);
    EXPECT_EQ(statFS.f_namemax, statInfo.f_namemax);
}

TEST_F(FsImplTest, flush) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillRepeatedly(Return(std::make_pair(location, storage)));
    EXPECT_EQ(0, client->flush("/path", &fileInfo));
}

TEST_F(FsImplTest, release) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _, _)).WillRepeatedly(Return(std::make_pair(location, storage)));
    EXPECT_CALL(*storageMapperMock, releaseFile("/path"));
    EXPECT_EQ(0, client->release("/path", &fileInfo));
}

TEST_F(FsImplTest, fsync) { // const char *path, int datasync, struct fuse_file_info *fi
    EXPECT_EQ(0, client->fsync("/path", 0, &fileInfo));
}

TEST_F(FsImplTest, setxattr) { // const char *path, const char *name, const char *value, size_t size, int flags
    EXPECT_EQ(-EIO, client->setxattr("/path", "key", "value", 10, 0));
}

TEST_F(FsImplTest, getxattr) { // const char *path, const char *name, char *value, size_t size
    char tmpBuff[32];
    EXPECT_EQ(-EIO, client->getxattr("/path", "key", tmpBuff, 32));
}

TEST_F(FsImplTest, listxattr) { // const char *path, char *list, size_t size
    char tmpBuff[32];
    EXPECT_EQ(-EIO, client->listxattr("/path", tmpBuff, 32));
}

TEST_F(FsImplTest, removexattr) { // const char *path, const char *name
    EXPECT_EQ(-EIO, client->removexattr("/path", "key"));
}

TEST_F(FsImplTest, opendir) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_CALL(*scheduler, post(_));
    EXPECT_EQ(0, client->opendir("/path", &fileInfo));
}

TEST_F(FsImplTest, readdir) { // const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo
    // Nearly imposible to test because most important action is handled by given FUSE filler func.
    // Mocking it is pointless because readdir wouldnt have any meaning.
}

TEST_F(FsImplTest, releasedir) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_EQ(0, client->releasedir("/path", &fileInfo));
}

TEST_F(FsImplTest, fsyncdir) { // const char *path, int datasync, struct fuse_file_info *fileInfo
    EXPECT_EQ(0, client->fsyncdir("/path", 0, &fileInfo));
}

TEST_F(FsImplTest, init) { // struct fuse_conn_info *conn
    struct fuse_conn_info info;
    EXPECT_EQ(0, client->init(&info));
}

TEST_F(FsImplTest, processEvent) {
    auto combinerMock = std::make_shared<MockEventStreamCombiner>(context);
    ASSERT_TRUE((bool) combinerMock);
    EventCommunicator communicator(context, combinerMock);
    EXPECT_CALL(*combinerMock, pushEventToProcess(_)).WillOnce(Return());
    EXPECT_CALL(*scheduler, schedule(_, _));
    std::shared_ptr<Event> event = Event::createMkdirEvent("some_file");

    ASSERT_TRUE((bool) event);
    communicator.processEvent(event);
}

struct FSImplBlockTest: public FsImplTest
{
    void SetUp() override
    {
        FsImplTest::SetUp();

        location.blocks.clear();
        trueStat.st_size = 100;

        ON_CALL(*metaCacheMock, getAttr("/file", _))
            .WillByDefault(DoAll(SetArgPointee<1>(trueStat), Return(true)));

        EXPECT_CALL(*storageMapperMock, getLocationInfo("/file", _, _))
            .WillRepeatedly(InvokeWithoutArgs([&] {
                 return std::make_pair(location, storage);
             }));
    }

    std::array<char, 10> buff;
};

TEST_F(FSImplBlockTest, shouldWaitForBlocksOnRead)
{
    EXPECT_CALL(*storageMapperMock, waitForBlock("/file", _, _))
        .WillOnce(Return(true));

    client->read("/file", buff.data(), 3, 0, &fileInfo);
}

TEST_F(FSImplBlockTest, shouldRequestAFileBlockIfMissing)
{
    EXPECT_CALL(*fslogicMock, requestFileBlock("/file", 0, _));
    client->read("/file", buff.data(), 4, 0, &fileInfo);

    location.blocks += boost::icl::discrete_interval<off_t>::closed(2, 5);
    EXPECT_CALL(*fslogicMock, requestFileBlock("/file", Le(3), _));
    client->read("/file", buff.data(), 4, 0, &fileInfo);
}

TEST_F(FSImplBlockTest, shouldNotBlockOnAvailableBlock)
{
    location.blocks += boost::icl::discrete_interval<off_t>::closed(0, 3);
    EXPECT_CALL(*fslogicMock, requestFileBlock("/file", _, _)).Times(0);
    client->read("/file", buff.data(), 4, 0, &fileInfo);
}

TEST_F(FSImplBlockTest, shouldImmediatelyReturnDataIfPossible)
{
    location.blocks += boost::icl::discrete_interval<off_t>::closed(0, 3);
    EXPECT_CALL(*helperMock, sh_read(_, _, 3, 0, _));
    client->read("/file", buff.data(), buff.size(), 0, &fileInfo);
}
