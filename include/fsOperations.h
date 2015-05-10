/**
* @file fsOperations.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_FS_OPERATIONS_H
#define ONECLIENT_FS_OPERATIONS_H

#include <fuse.h>

int wrap_access(const char *path, int mask);
int wrap_getattr(const char *path, struct stat *statbuf);
int wrap_readlink(const char *path, char *link, size_t size);
int wrap_mknod(const char *path, mode_t mode, dev_t dev);
int wrap_mkdir(const char *path, mode_t mode);
int wrap_unlink(const char *path);
int wrap_rmdir(const char *path);
int wrap_symlink(const char *path, const char *link);
int wrap_rename(const char *path, const char *newpath);
int wrap_chmod(const char *path, mode_t mode);
int wrap_chown(const char *path, uid_t uid, gid_t gid);
int wrap_truncate(const char *path, off_t newSize);
int wrap_utime(const char *path, struct utimbuf *ubuf);
int wrap_open(const char *path, struct fuse_file_info *fileInfo);
int wrap_read(const char *path, char *buf, size_t size, off_t offset,
    struct fuse_file_info *fileInfo);
int wrap_write(const char *path, const char *buf, size_t size, off_t offset,
    struct fuse_file_info *fileInfo);
int wrap_statfs(const char *path, struct statvfs *statInfo);
int wrap_flush(const char *path, struct fuse_file_info *fileInfo);
int wrap_release(const char *path, struct fuse_file_info *fileInfo);
int wrap_fsync(const char *path, int datasync, struct fuse_file_info *fi);
int wrap_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
    off_t offset, struct fuse_file_info *fileInfo);
int wrap_opendir(const char *path, struct fuse_file_info *fileInfo);
int wrap_releasedir(const char *path, struct fuse_file_info *fileInfo);
int wrap_fsyncdir(
    const char *path, int datasync, struct fuse_file_info *fileInfo);
void *init(struct fuse_conn_info *conn);

struct fuse_operations fuseOperations();

#endif // ONECLIENT_FS_OPERATIONS_H
