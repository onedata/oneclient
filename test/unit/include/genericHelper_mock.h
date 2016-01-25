/**
 * @file genericHelper_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef GENERIC_HELPER_MOCK_H
#define GENERIC_HELPER_MOCK_H

#include "helpers/IStorageHelper.h"

#include <gmock/gmock.h>

class MockGenericHelper : public one::helpers::IStorageHelper {
public:
    MOCK_METHOD2(sh_getattr, int(const char *path, struct stat *stbuf));
    MOCK_METHOD2(sh_access, int(const char *path, int mask));
    MOCK_METHOD3(sh_readlink, int(const char *path, char *buf, size_t size));
    MOCK_METHOD5(
        sh_readdir, int(const char *path, void *buf, fuse_fill_dir_t filler,
                        off_t offset, struct fuse_file_info *fi));
    MOCK_METHOD3(sh_mknod, int(const char *path, mode_t mode, dev_t rdev));
    MOCK_METHOD2(sh_mkdir, int(const char *path, mode_t mode));
    MOCK_METHOD1(sh_unlink, int(const char *path));
    MOCK_METHOD1(sh_rmdir, int(const char *path));
    MOCK_METHOD2(sh_symlink, int(const char *from, const char *to));
    MOCK_METHOD2(sh_rename, int(const char *from, const char *to));
    MOCK_METHOD2(sh_link, int(const char *from, const char *to));
    MOCK_METHOD2(sh_chmod, int(const char *path, mode_t mode));
    MOCK_METHOD3(sh_chown, int(const char *path, uid_t uid, gid_t gid));
    MOCK_METHOD2(sh_truncate, int(const char *path, off_t size));

    MOCK_METHOD2(sh_open, int(const char *path, struct fuse_file_info *fi));
    MOCK_METHOD2(sh_flush, int(const char *path, struct fuse_file_info *fi));
    MOCK_METHOD5(sh_read, int(const char *path, char *buf, size_t size,
                              off_t offset, struct fuse_file_info *fi));
    MOCK_METHOD5(sh_write, int(const char *path, const char *buf, size_t size,
                               off_t offset, struct fuse_file_info *fi));
    MOCK_METHOD2(sh_statfs, int(const char *path, struct statvfs *stbuf));
    MOCK_METHOD2(sh_release, int(const char *path, struct fuse_file_info *fi));
    MOCK_METHOD3(sh_fsync,
        int(const char *path, int isdatasync, struct fuse_file_info *fi));
};

#endif // GENERIC_HELPER_MOCK_H
