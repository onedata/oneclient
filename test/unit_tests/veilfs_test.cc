/**
 * @file veilfs_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.h"
#include "veilfs_proxy.h"
#include "fslogicProxy_proxy.h"
#include "messageBuilder_mock.h"
#include "config_mock.h"
#include "events_mock.h"
#include "jobScheduler_mock.h"
#include "storageHelperFactory_fake.h"
#include "fslogicProxy_mock.h"
#include "metaCache_mock.h"
#include "storageMapper_mock.h"
#include "veilErrors.h"
#include "events.h"
#include "events_mock.h"

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class VeilFSTest 
    : public ::testing::Test
{
public:
    COMMON_DEFS();
    boost::shared_ptr<ProxyVeilFS> client;

    boost::shared_ptr<MockJobScheduler> jobSchedulerMock;
    boost::shared_ptr<MockFslogicProxy> fslogicMock;
    boost::shared_ptr<MockMetaCache> metaCacheMock; 
    boost::shared_ptr<MockStorageMapper> storageMapperMock;
    boost::shared_ptr<MockGenericHelper> helperMock;
    boost::shared_ptr<FakeStorageHelperFactory> factoryFake;
    boost::shared_ptr<MockEventCommunicator> eventCommunicatorMock;

    struct fuse_file_info fileInfo;
    struct stat trueStat;
    FileAttr trueAttr;
    locationInfo location;
    storageInfo storage;

    virtual void SetUp() {
        COMMON_SETUP();
        jobSchedulerMock.reset(new MockJobScheduler());
        fslogicMock.reset(new MockFslogicProxy());
        metaCacheMock.reset(new MockMetaCache());
        storageMapperMock.reset(new MockStorageMapper(fslogicMock));
        helperMock.reset(new MockGenericHelper());
        factoryFake.reset(new FakeStorageHelperFactory());
        eventCommunicatorMock.reset(new MockEventCommunicator());

        EXPECT_CALL(*fslogicMock, pingCluster()).WillRepeatedly(Return());
        EXPECT_CALL(*config, getInt(ALIVE_META_CONNECTIONS_COUNT_OPT)).WillRepeatedly(Return(0));
        EXPECT_CALL(*config, getInt(ALIVE_DATA_CONNECTIONS_COUNT_OPT)).WillRepeatedly(Return(0));
        EXPECT_CALL(*config, isSet(FUSE_ID_OPT)).WillRepeatedly(Return(false));
        
        VeilFS::staticDestroy();
        VeilFS::setConnectionPool(connectionPool);
        client.reset(new ProxyVeilFS("/root", config, 
                        jobSchedulerMock,
                        fslogicMock, 
                        metaCacheMock,
                        storageMapperMock,
                        factoryFake,
                        eventCommunicatorMock));

        factoryFake->presetMock = helperMock;

        location.fileId = "fileid";
        location.storageId = 1;
        storage.storageHelperName = "sh_name";
        storage.storageHelperArgs.push_back("arg1");
        storage.storageHelperArgs.push_back("arg2");

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

        EXPECT_CALL(*jobSchedulerMock, addTask(_)).WillRepeatedly(Return());
    }

    virtual void TearDown() {
        Mock::VerifyAndClearExpectations(storageMapperMock.get());
        COMMON_CLEANUP();   
    }
};

TEST_F(VeilFSTest, Instantiate) {
    EXPECT_EQ(jobSchedulerMock.get(), VeilFS::getScheduler().get());
    EXPECT_EQ(config.get(), VeilFS::getConfig().get());
    EXPECT_TRUE(client->getPushListener().get());
}

TEST_F(VeilFSTest, translateError) {
    EXPECT_EQ(0, veil::translateError(VOK));
    EXPECT_EQ(-ENOENT, veil::translateError(VENOENT));
    EXPECT_EQ(-EACCES, veil::translateError(VEACCES));
    EXPECT_EQ(-EEXIST, veil::translateError(VEEXIST));
    EXPECT_EQ(-EIO, veil::translateError(VEIO));

    EXPECT_EQ(-EIO, veil::translateError("unknown"));
    EXPECT_EQ(-EIO, veil::translateError("other unknown"));
}

TEST_F(VeilFSTest, access) { // const char *path, int mask
    EXPECT_EQ(0, client->access("/path", 1234));
}

TEST_F(VeilFSTest, getattrCache) { // const char *path, struct stat *statbuf
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

TEST_F(VeilFSTest, getattrNoCluster) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(Return(false));
    EXPECT_EQ(-EIO, client->getattr("/path", &statbuf));
}

TEST_F(VeilFSTest, getattr) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    
    EXPECT_CALL(*config, getBool(ENABLE_DIR_PREFETCH_OPT)).WillOnce(Return(true));
    EXPECT_CALL(*config, getBool(ENABLE_ATTR_CACHE_OPT)).WillOnce(Return(true));
    EXPECT_CALL(*jobSchedulerMock, addTask(_)).WillOnce(Return());

    trueAttr.set_type("DIR");
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(Return(false));
    EXPECT_EQ(-EIO, client->getattr("/path", &statbuf));

    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgReferee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*metaCacheMock, addAttr("/path", Truly(bind(identityEqual<struct stat>, boost::cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));

    EXPECT_EQ(trueAttr.atime(), statbuf.st_atime);
    EXPECT_EQ(trueAttr.ctime(), statbuf.st_ctime);
    EXPECT_EQ(trueAttr.mtime(), statbuf.st_mtime);
    
    EXPECT_EQ(trueAttr.size(), statbuf.st_size);
    EXPECT_EQ(trueAttr.gid(), statbuf.st_gid);
    EXPECT_EQ(0, statbuf.st_uid); // Its root
    
    EXPECT_EQ(trueAttr.mode() | S_IFDIR, statbuf.st_mode);

    trueAttr.set_type("LNK");
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgReferee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*metaCacheMock, addAttr("/path", Truly(bind(identityEqual<struct stat>, boost::cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));

    EXPECT_EQ(trueAttr.mode() | S_IFLNK, statbuf.st_mode);

    trueAttr.set_type("REG");
    EXPECT_CALL(*jobSchedulerMock, addTask(_)).WillOnce(Return());
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgReferee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*metaCacheMock, addAttr("/path", Truly(bind(identityEqual<struct stat>, boost::cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));

    EXPECT_EQ(trueAttr.mode() | S_IFREG, statbuf.st_mode);    
}
 
TEST_F(VeilFSTest, readlink) { // const char *path, char *link, size_t size
    char link[5];

    EXPECT_CALL(*fslogicMock, getLink("/path")).WillOnce(Return(make_pair(VENOENT, "")));
    EXPECT_EQ(-ENOENT, client->readlink("/path", link, 5));

    EXPECT_CALL(*fslogicMock, getLink("/path1")).WillOnce(Return(make_pair(VOK, "1234")));
    EXPECT_EQ(0, client->readlink("/path1", link, 5));
    EXPECT_EQ("1234", string(link));

    EXPECT_CALL(*fslogicMock, getLink("/path2")).WillOnce(Return(make_pair(VOK, "12345")));
    EXPECT_EQ(0, client->readlink("/path2", link, 5));
    EXPECT_EQ("1234", string(link));

    EXPECT_CALL(*fslogicMock, getLink("/path3")).WillOnce(Return(make_pair(VOK, "123456")));
    EXPECT_EQ(0, client->readlink("/path3", link, 5));
    EXPECT_EQ("1234", string(link));

    EXPECT_CALL(*fslogicMock, getLink("/path4")).WillOnce(Return(make_pair(VOK, "/1234")));
    EXPECT_EQ(0, client->readlink("/path4", link, 5));
    EXPECT_EQ("/roo", string(link));
}
 
TEST_F(VeilFSTest, mknod) { // const char *path, mode_t mode, dev_t dev
    FileLocation newLoc;
    dev_t dev;
    EXPECT_CALL(*metaCacheMock, clearAttr("/path")).Times(AtLeast(3));

    newLoc.set_answer(VOK);
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck(_)).Times(0);
    EXPECT_EQ(-EIO, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VEACCES);
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));   
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck(_)).Times(0);
    EXPECT_EQ(-EACCES, client->mknod("/path", 123 | S_IFREG, dev)); 

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return()); 
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck(_)).Times(0);
    EXPECT_EQ(-EACCES, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return()); 
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillOnce(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_mknod(StrEq("fileid"), 123 | S_IFREG, dev));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck("/path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VEEXIST);
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck(_)).Times(0);
    EXPECT_EQ(-EEXIST, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return()); 
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillOnce(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_mknod(StrEq("fileid"), 123 | S_IFREG, dev)).WillOnce(Return(-EEXIST));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck("/path")).WillOnce(Return(VEIO));
    EXPECT_EQ(-EIO, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgReferee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return()); 
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillOnce(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_mknod(StrEq("fileid"), 123 | S_IFREG, dev)).WillOnce(Return(-EEXIST));
    EXPECT_CALL(*fslogicMock, sendFileCreatedAck("/path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->mknod("/path", 123 | S_IFREG, dev)); 

}
 
TEST_F(VeilFSTest, mkdir) { // const char *path, mode_t mode
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*metaCacheMock, clearAttr("/")).WillRepeatedly(Return());
    EXPECT_CALL(*fslogicMock, createDir("/path", 123)).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->mkdir("/path", 123 | S_IFDIR));

    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*fslogicMock, createDir("/path", 123)).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->mkdir("/path", 123 | S_IFDIR));
}
 
TEST_F(VeilFSTest, unlink) { // const char *path
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path")).Times(AtLeast(3));

    struct stat st;
    st.st_mode |= S_IFLNK;
    FileAttr attrs;
    attrs.set_type("LNK");

    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(st), Return(true)));
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*storageMapperMock, getLocationInfo(_, _)).Times(0);
    EXPECT_CALL(*helperMock, sh_unlink(_)).Times(0);
    EXPECT_EQ(0, client->unlink("/path"));

    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(st), Return(false)));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgReferee<1>(attrs), Return(true)));
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*storageMapperMock, getLocationInfo(_, _)).Times(0);
    EXPECT_CALL(*helperMock, sh_unlink(_)).Times(0);
    EXPECT_EQ(0, client->unlink("/path"));

    attrs.set_type("REG"); 
    EXPECT_CALL(*metaCacheMock, getAttr("/path", _)).WillRepeatedly(DoAll(SetArgPointee<1>(st), Return(false)));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillRepeatedly(DoAll(SetArgReferee<1>(attrs), Return(true)));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).Times(0);
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->unlink("/path"));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).Times(0);
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_unlink(StrEq("fileid"))).WillOnce(Return(-ENOENT));
    EXPECT_EQ(-ENOENT, client->unlink("/path"));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_unlink(StrEq("fileid"))).WillOnce(Return(0));
    EXPECT_EQ(0, client->unlink("/path"));
}
 
TEST_F(VeilFSTest, rmdir) { // const char *path
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*metaCacheMock, clearAttr("/")).WillRepeatedly(Return());
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->rmdir("/path"));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_EQ(0, client->rmdir("/path"));
}
 
TEST_F(VeilFSTest, symlink) { // const char *path, const char *link
    EXPECT_CALL(*fslogicMock, createLink("/link", "/path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->symlink("/path", "/link"));

    EXPECT_CALL(*fslogicMock, createLink("/link", "path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->symlink("path", "/link"));

    EXPECT_CALL(*fslogicMock, createLink("/link", "/path")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->symlink("/root/path", "/link"));

    EXPECT_CALL(*fslogicMock, createLink("/link", "/path")).WillOnce(Return(VENOENT));
    EXPECT_EQ(-ENOENT, client->symlink("/path", "/link"));
}
 
TEST_F(VeilFSTest, rename) { // const char *path, const char *newpath
    EXPECT_CALL(*fslogicMock, renameFile("/path", "/new/path")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->rename("/path", "/new/path"));

    EXPECT_CALL(*fslogicMock, renameFile("/path", "/new/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_EQ(0, client->rename("/path", "/new/path"));
}
 
TEST_F(VeilFSTest, link) { // const char *path, const char *newpath
    EXPECT_EQ(-ENOTSUP, client->link("/path", "/link"));
}
 
TEST_F(VeilFSTest, chmod) { // const char *path, mode_t mode
    mode_t dirMode = (123 | S_IFDIR);
    mode_t regMode = (123 | S_IFREG);
    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VENOENT));
    EXPECT_EQ(-ENOENT, client->chmod("/path", regMode));

    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_EQ(0, client->chmod("/path", dirMode));

    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->chmod("/path", regMode));

    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_chmod(StrEq("fileid"), regMode)).WillOnce(Return(-EACCES));
    EXPECT_EQ(-EACCES, client->chmod("/path", regMode));

    EXPECT_CALL(*fslogicMock, changeFilePerms("/path", 123)).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_chmod(StrEq("fileid"), regMode)).WillOnce(Return(0));
    EXPECT_EQ(0, client->chmod("/path", regMode));
}
 
TEST_F(VeilFSTest, chown) { // const char *path, uid_t uid, gid_t gid
    EXPECT_CALL(*metaCacheMock, clearAttr("/path")).WillRepeatedly(Return());
    
    EXPECT_EQ(0, client->chown("/path", -1,-1)); // Dont change perms
    
    EXPECT_CALL(*fslogicMock, changeFileGroup(_, _, _)).Times(0);
    EXPECT_CALL(*fslogicMock, changeFileOwner("/path", 0, "root")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->chown("/path", 0, -1));
    
    EXPECT_CALL(*fslogicMock, changeFileOwner("/path", 0, "root")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", 0, -1));
    
    EXPECT_CALL(*fslogicMock, changeFileOwner("/path", 64231, "")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", 64231, -1));
    
    EXPECT_CALL(*fslogicMock, changeFileOwner(_, _, _)).Times(0);
    EXPECT_CALL(*fslogicMock, changeFileGroup("/path", 0, "root")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->chown("/path", -1, 0));
    
    EXPECT_CALL(*fslogicMock, changeFileGroup("/path", 0, "root")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", -1, 0));
    
    EXPECT_CALL(*fslogicMock, changeFileGroup("/path", 54321, "")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", -1, 54321));
    
    
    EXPECT_CALL(*fslogicMock, changeFileOwner("/path", 0, "root")).WillOnce(Return(VOK));
    EXPECT_CALL(*fslogicMock, changeFileGroup("/path", 54321, "")).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->chown("/path", 0, 54321));
    
}
 
TEST_F(VeilFSTest, truncate) { // const char *path, off_t newSize

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->truncate("/path", 10));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    
    EXPECT_CALL(*helperMock, sh_truncate(StrEq("fileid"), _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, client->truncate("/path", 10));

    EXPECT_CALL(*metaCacheMock, updateSize("/path", 10)).WillOnce(Return(true));

    EXPECT_CALL(*helperMock, sh_truncate(StrEq("fileid"), _)).WillOnce(Return(0));
    EXPECT_EQ(0, client->truncate("/path", 10));
}
 
TEST_F(VeilFSTest, utime) { // const char *path, struct utimbuf *ubuf
    struct utimbuf ubuf;
    
    EXPECT_CALL(*jobSchedulerMock, addTask(_)).WillOnce(Return());
    
    EXPECT_EQ(0, client->utime("/path", &ubuf));
}
 
TEST_F(VeilFSTest, open) { // const char *path, struct fuse_file_info *fileInfo
    
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->open("/path", &fileInfo));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    
    EXPECT_CALL(*helperMock, sh_open(StrEq("fileid"), _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, client->open("/path", &fileInfo));

    EXPECT_CALL(*helperMock, sh_open(StrEq("fileid"), _)).WillOnce(Return(0));
    fileInfo.flags |= O_RDWR;
    EXPECT_EQ(0, client->open("/path", &fileInfo));
}
 
TEST_F(VeilFSTest, read) { // const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo
    char tmpBuff[4];

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->read("/path", tmpBuff, 4, 0, &fileInfo));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    
    EXPECT_CALL(*helperMock, sh_read(StrEq("fileid"), tmpBuff, 4, 0, _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, client->read("/path", tmpBuff, 4, 0, &fileInfo));

    EXPECT_CALL(*helperMock, sh_read(StrEq("fileid"), tmpBuff, 4, 0, _)).WillOnce(Return(0));
    EXPECT_EQ(0, client->read("/path", tmpBuff, 4, 0, &fileInfo));
}
 
TEST_F(VeilFSTest, write) { // const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fileInfo
    
    // TODO: "userId" is hardcoded
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->write("/path", "abcd", 4, 0, &fileInfo));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    
    EXPECT_CALL(*helperMock, sh_write(StrEq("fileid"), StrEq("abcd"), 4, 0, _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, client->write("/path", "abcd", 4, 0, &fileInfo));

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
    EXPECT_CALL(*eventCommunicatorMock, processEvent(_)).Times(1);
    EXPECT_EQ(4, client->write("/path", "abcd", 4, 2, &fileInfo));
    
}
 
TEST_F(VeilFSTest, statfs) { // const char *path, struct statvfs *statInfo
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

    EXPECT_CALL(*fslogicMock, getStatFS()).WillOnce(Return(make_pair(VEREMOTEIO, statFS)));
    EXPECT_EQ(-EREMOTEIO, client->statfs("/path", &statInfo));

    EXPECT_CALL(*fslogicMock, getStatFS()).WillOnce(Return(make_pair(VOK, statFS)));
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

TEST_F(VeilFSTest, flush) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    EXPECT_EQ(0, client->flush("/path", &fileInfo));
}
 
TEST_F(VeilFSTest, release) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    EXPECT_CALL(*storageMapperMock, releaseFile("/path")).Times(1);
    EXPECT_EQ(0, client->release("/path", &fileInfo));
}
 
TEST_F(VeilFSTest, fsync) { // const char *path, int datasync, struct fuse_file_info *fi
    EXPECT_EQ(0, client->fsync("/path", 0, &fileInfo));
}
 
TEST_F(VeilFSTest, setxattr) { // const char *path, const char *name, const char *value, size_t size, int flags
    EXPECT_EQ(-EIO, client->setxattr("/path", "key", "value", 10, 0));
}
 
TEST_F(VeilFSTest, getxattr) { // const char *path, const char *name, char *value, size_t size
    char tmpBuff[32];
    EXPECT_EQ(-EIO, client->getxattr("/path", "key", tmpBuff, 32));
}
 
TEST_F(VeilFSTest, listxattr) { // const char *path, char *list, size_t size
    char tmpBuff[32];
    EXPECT_EQ(-EIO, client->listxattr("/path", tmpBuff, 32));
}
 
TEST_F(VeilFSTest, removexattr) { // const char *path, const char *name
    EXPECT_EQ(-EIO, client->removexattr("/path", "key"));
}
 
TEST_F(VeilFSTest, opendir) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_CALL(*jobSchedulerMock, addTask(_)).WillOnce(Return());
    EXPECT_EQ(0, client->opendir("/path", &fileInfo));
}
 
TEST_F(VeilFSTest, readdir) { // const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo
    // Nearly imposible to test because most important action is handled by given FUSE filler func. 
    // Mocking it is pointless because readdir wouldnt have any meaning.
}
 
TEST_F(VeilFSTest, releasedir) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_EQ(0, client->releasedir("/path", &fileInfo));
}
 
TEST_F(VeilFSTest, fsyncdir) { // const char *path, int datasync, struct fuse_file_info *fileInfo
    EXPECT_EQ(0, client->fsyncdir("/path", 0, &fileInfo));
}
 
TEST_F(VeilFSTest, init) { // struct fuse_conn_info *conn
    struct fuse_conn_info info;
    EXPECT_EQ(0, client->init(&info));
}
 
TEST_F(VeilFSTest, processEvent) {
    shared_ptr<MockEventStreamCombiner> combinerMock(new MockEventStreamCombiner());
    ASSERT_TRUE((bool) combinerMock);
    EventCommunicator communicator(combinerMock);
    EXPECT_CALL(*combinerMock, pushEventToProcess(_)).WillOnce(Return());
    EXPECT_CALL(*jobSchedulerMock, addTask(_)).WillOnce(Return());
    shared_ptr<Event> event = Event::createMkdirEvent("userId", "some_file");

    ASSERT_TRUE((bool) event);
    communicator.processEvent(event);
}