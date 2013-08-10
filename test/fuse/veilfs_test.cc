/**
 * @file veilfs_test.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#include "testCommon.hh"
#include "fslogicProxy_proxy.hh"
#include "messageBuilder_mock.hh"
#include "config_mock.hh"
#include "jobScheduler_mock.hh"
#include "storageHelperFactory_fake.hh"
#include "fslogicProxy_mock.hh"
#include "metaCache_mock.hh"
#include "storageMapper_mock.hh"

INIT_AND_RUN_ALL_TESTS(); // TEST RUNNER !

// TEST definitions below

class VeilFSTest 
    : public ::testing::Test
{
public:
    COMMON_DEFS();
    shared_ptr<VeilFS> client;

    shared_ptr<MockJobScheduler> jobSchedulerMock;
    shared_ptr<MockFslogicProxy> fslogicMock;
    shared_ptr<MockMetaCache> metaCacheMock; 
    shared_ptr<MockStorageMapper> storageMapperMock;
    shared_ptr<MockGenericHelper> helperMock;
    shared_ptr<FakeStorageHelperFactory> factoryFake;

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
        storageMapperMock.reset(new MockStorageMapper(*fslogicMock));
        helperMock.reset(new MockGenericHelper());
        factoryFake.reset(new FakeStorageHelperFactory());

        client.reset(new VeilFS("/", config, 
                        jobSchedulerMock,
                        fslogicMock, 
                        metaCacheMock,
                        storageMapperMock,
                        factoryFake));

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
        trueAttr.set_gid(12);
        trueAttr.set_uid(13);
    }

    virtual void TearDown() {
        Mock::VerifyAndClearExpectations(storageMapperMock.get());
        COMMON_CLEANUP();   
    }
};

TEST_F(VeilFSTest, Instantiate) {
    EXPECT_EQ(jobSchedulerMock.get(), VeilFS::getScheduler().get());
    EXPECT_EQ(config.get(), VeilFS::getConfig().get());
}

TEST_F(VeilFSTest, translateError) {
    EXPECT_EQ(0, VeilFS::translateError(VOK));
    EXPECT_EQ(-ENOENT, VeilFS::translateError(VENOENT));
    EXPECT_EQ(-EACCES, VeilFS::translateError(VEACCES));
    EXPECT_EQ(-EEXIST, VeilFS::translateError(VEEXIST));
    EXPECT_EQ(-EIO, VeilFS::translateError(VEIO));

    EXPECT_EQ(-EIO, VeilFS::translateError("unknown"));
    EXPECT_EQ(-EIO, VeilFS::translateError("other unknown"));
}

TEST_F(VeilFSTest, access) { // const char *path, int mask
    EXPECT_EQ(0, client->access("/path", 1234));
}
 
TEST_F(VeilFSTest, getattrRegInCache) { // const char *path, struct stat *statbuf
    struct stat statbuf;

    // Call path #1
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).Times(AtLeast(1)).WillRepeatedly(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_getattr(StrEq("fileid"), _)).WillOnce(Return(-EACCES));
    EXPECT_EQ(-EACCES, client->getattr("/path", &statbuf));

    // Call path #2
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).Times(AtLeast(1)).WillRepeatedly(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_getattr(StrEq("fileid"), _)).WillOnce(DoAll(SetArgPointee<1>(trueStat), Return(0)));
    EXPECT_CALL(*metaCacheMock, addAttr("/path", Truly(bind(identityEqual<struct stat>, cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));
    EXPECT_EQ(trueStat.st_atime, statbuf.st_atime);
    EXPECT_EQ(trueStat.st_ctime, statbuf.st_ctime);
    EXPECT_EQ(trueStat.st_mtime, statbuf.st_mtime);
    EXPECT_EQ(trueStat.st_mode, statbuf.st_mode);
    EXPECT_EQ(trueStat.st_gid, statbuf.st_gid);
    EXPECT_EQ(trueStat.st_uid, statbuf.st_uid);
    EXPECT_EQ(trueStat.st_size, statbuf.st_size);
}

TEST_F(VeilFSTest, getattrRegNotInCachePath1) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    trueAttr.set_type("REG");

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).Times(AtLeast(1)).WillRepeatedly(Throw(VeilException(VENOENT)));
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

TEST_F(VeilFSTest, getattrRegNotInCachePath2) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    trueAttr.set_type("REG");

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).Times(AtLeast(1)).WillRepeatedly(Throw(VeilException(VENOENT)));
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(Return(false));
    EXPECT_EQ(-EIO, client->getattr("/path", &statbuf));
}

TEST_F(VeilFSTest, getattrRegNotInCachePath3) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    trueAttr.set_type("REG");

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).Times(AtLeast(1)).WillRepeatedly(Throw(VeilException(VENOENT)));
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*storageMapperMock, findLocation("/path")).WillOnce(Return(VENOENT));
    EXPECT_EQ(-ENOENT, client->getattr("/path", &statbuf));
}

TEST_F(VeilFSTest, getattrRegNotInCachePath4) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    trueAttr.set_type("REG");

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).Times(AtLeast(1)).WillRepeatedly(Throw(VeilException(VENOENT)));
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*storageMapperMock, findLocation("/path")).WillOnce(Return(VOK));
    EXPECT_EQ(-EIO, client->getattr("/path", &statbuf));
}

TEST_F(VeilFSTest, getattrRegNotInCachePath5) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    trueAttr.set_type("REG");

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillOnce(Throw(VeilException(VENOENT))).WillOnce(Return(make_pair(location, storage)));
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*storageMapperMock, findLocation("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*helperMock, sh_getattr(StrEq("fileid"), _)).WillOnce(DoAll(SetArgPointee<1>(trueStat), Return(0)));
    EXPECT_CALL(*metaCacheMock, addAttr("/path", Truly(bind(identityEqual<struct stat>, cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));
    EXPECT_EQ(trueStat.st_atime, statbuf.st_atime);
    EXPECT_EQ(trueStat.st_ctime, statbuf.st_ctime);
    EXPECT_EQ(trueStat.st_mtime, statbuf.st_mtime);
    EXPECT_EQ(trueStat.st_mode, statbuf.st_mode);
    EXPECT_EQ(trueStat.st_gid, statbuf.st_gid);
    EXPECT_EQ(trueStat.st_uid, statbuf.st_uid);
    EXPECT_EQ(trueStat.st_size, statbuf.st_size);
}

TEST_F(VeilFSTest, getattrRegNotInCachePath6) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    trueAttr.set_type("REG");

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillOnce(Throw(VeilException(VENOENT))).WillOnce(Return(make_pair(location, storage)));
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*storageMapperMock, findLocation("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*helperMock, sh_getattr(StrEq("fileid"), _)).WillOnce(Return(-EACCES));
    EXPECT_EQ(-EACCES, client->getattr("/path", &statbuf));
}

TEST_F(VeilFSTest, getattrDir) { // const char *path, struct stat *statbuf
    struct stat statbuf;
    trueAttr.set_type("DIR");

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillRepeatedly(Throw(VeilException(VEIO)));
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(Return(false));
    EXPECT_EQ(-EIO, client->getattr("/path", &statbuf));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillRepeatedly(Throw(VeilException(VEIO)));
    EXPECT_CALL(*metaCacheMock, getAttr("/path", &statbuf)).WillOnce(Return(false));
    EXPECT_CALL(*fslogicMock, getFileAttr("/path", _)).WillOnce(DoAll(SetArgPointee<1>(trueAttr), Return(true)));
    EXPECT_CALL(*metaCacheMock, addAttr("/path", Truly(bind(identityEqual<struct stat>, cref(statbuf), _1))));
    EXPECT_EQ(0, client->getattr("/path", &statbuf));

    EXPECT_EQ(trueAttr.atime(), statbuf.st_atime);
    EXPECT_EQ(trueAttr.ctime(), statbuf.st_ctime);
    EXPECT_EQ(trueAttr.mtime(), statbuf.st_mtime);
    EXPECT_EQ(trueAttr.mode() | S_IFDIR, statbuf.st_mode);    
}
 
TEST_F(VeilFSTest, readlink) { // const char *path, char *link, size_t size
    char link[32];
    EXPECT_EQ(-EIO, client->readlink("/path", link, 32));
}
 
TEST_F(VeilFSTest, mknod) { // const char *path, mode_t mode, dev_t dev
    FileLocation newLoc;
    dev_t dev;
    EXPECT_CALL(*metaCacheMock, clearAttr("/path")).Times(AtLeast(3));

    newLoc.set_answer(VOK);
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(Return(false));
    EXPECT_EQ(-EIO, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VEACCES);
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgPointee<2>(newLoc), Return(true)));   
    EXPECT_EQ(-EACCES, client->mknod("/path", 123 | S_IFREG, dev)); 

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgPointee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return()); 
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->mknod("/path", 123 | S_IFREG, dev));

    newLoc.set_answer(VOK);
    newLoc.set_file_id("fid");
    EXPECT_CALL(*fslogicMock, getNewFileLocation("/path", 123, _)).WillOnce(DoAll(SetArgPointee<2>(newLoc), Return(true)));
    EXPECT_CALL(*storageMapperMock, addLocation("/path", Property(&FileLocation::file_id, StrEq("fid")))).WillOnce(Return()); 
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", _)).WillOnce(Return(make_pair(location, storage)));
    EXPECT_CALL(*helperMock, sh_mknod(StrEq("fileid"), 123 | S_IFREG, dev));

    EXPECT_EQ(0, client->mknod("/path", 123 | S_IFREG, dev));

}
 
TEST_F(VeilFSTest, mkdir) { // const char *path, mode_t mode
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*fslogicMock, createDir("/path", 123)).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->mkdir("/path", 123 | S_IFDIR));

    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_CALL(*fslogicMock, createDir("/path", 123)).WillOnce(Return(VOK));
    EXPECT_EQ(0, client->mkdir("/path", 123 | S_IFDIR));
}
 
TEST_F(VeilFSTest, unlink) { // const char *path
    EXPECT_CALL(*metaCacheMock, clearAttr("/path")).Times(AtLeast(3));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VENOENT));
    EXPECT_EQ(-ENOENT, client->unlink("/path"));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->unlink("/path"));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
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
    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->rmdir("/path"));

    EXPECT_CALL(*fslogicMock, deleteFile("/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_EQ(0, client->rmdir("/path"));
}
 
TEST_F(VeilFSTest, symlink) { // const char *path, const char *link
    EXPECT_EQ(-EIO, client->symlink("/path", "/link"));
}
 
TEST_F(VeilFSTest, rename) { // const char *path, const char *newpath
    EXPECT_CALL(*fslogicMock, renameFile("/path", "/new/path")).WillOnce(Return(VEACCES));
    EXPECT_EQ(-EACCES, client->rename("/path", "/new/path"));

    EXPECT_CALL(*fslogicMock, renameFile("/path", "/new/path")).WillOnce(Return(VOK));
    EXPECT_CALL(*metaCacheMock, clearAttr("/path"));
    EXPECT_EQ(0, client->rename("/path", "/new/path"));
}
 
TEST_F(VeilFSTest, link) { // const char *path, const char *newpath
    EXPECT_EQ(-EIO, client->link("/path", "/link"));
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
    EXPECT_EQ(-EIO, client->chown("/path", 6, 5));
}
 
TEST_F(VeilFSTest, truncate) { // const char *path, off_t newSize
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->truncate("/path", 10));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    
    EXPECT_CALL(*helperMock, sh_truncate(StrEq("fileid"), _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, client->truncate("/path", 10));

    EXPECT_CALL(*helperMock, sh_truncate(StrEq("fileid"), _)).WillOnce(Return(0));
    EXPECT_EQ(0, client->truncate("/path", 10));
}
 
TEST_F(VeilFSTest, utime) { // const char *path, struct utimbuf *ubuf
    struct utimbuf ubuf;
    EXPECT_EQ(0, client->utime("/path", &ubuf));
}
 
TEST_F(VeilFSTest, open) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->open("/path", &fileInfo));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    
    EXPECT_CALL(*helperMock, sh_open(StrEq("fileid"), _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, client->open("/path", &fileInfo));

    EXPECT_CALL(*helperMock, sh_open(StrEq("fileid"), _)).WillOnce(Return(0));
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
    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillOnce(Throw(VeilException(VEACCES)));
    EXPECT_EQ(-EACCES, client->write("/path", "abcd", 4, 0, &fileInfo));

    EXPECT_CALL(*storageMapperMock, getLocationInfo("/path", true)).WillRepeatedly(Return(make_pair(location, storage)));
    
    EXPECT_CALL(*helperMock, sh_write(StrEq("fileid"), StrEq("abcd"), 4, 0, _)).WillOnce(Return(-EEXIST));
    EXPECT_EQ(-EEXIST, client->write("/path", "abcd", 4, 0, &fileInfo));

    EXPECT_CALL(*helperMock, sh_write(StrEq("fileid"), StrEq("abcd"), 4, 0, _)).WillOnce(Return(0));
    EXPECT_EQ(0, client->write("/path", "abcd", 4, 0, &fileInfo));
    
}
 
TEST_F(VeilFSTest, statfs) { // const char *path, struct statvfs *statInfo
    struct statvfs statInfo;
    EXPECT_EQ(-EIO, client->statfs("/path", &statInfo));
}
 
TEST_F(VeilFSTest, flush) { // const char *path, struct fuse_file_info *fileInfo
    EXPECT_EQ(0, client->flush("/path", &fileInfo));
}
 
TEST_F(VeilFSTest, release) { // const char *path, struct fuse_file_info *fileInfo
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
 