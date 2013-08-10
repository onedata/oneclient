/**
 * @file IStorageHelper.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_HELPER_I_HH
#define STORAGE_HELPER_I_HH 1

#include <unistd.h>
#include <fcntl.h>
#include <fuse.h>

/**
 * The IStorageHelper interface.
 * Base class of all storage helpers. Unifies their interface.
 * All callback have their equivalent in FUSE API and should be used in that matter.
 */
class IStorageHelper {
	public:
        virtual ~IStorageHelper() {};

        virtual int sh_getattr(const char *path, struct stat *stbuf) = 0;
        virtual int sh_access(const char *path, int mask) = 0;
        virtual int sh_readlink(const char *path, char *buf, size_t size) = 0;
        virtual int sh_readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fi) = 0;
        virtual int sh_mknod(const char *path, mode_t mode, dev_t rdev) = 0;
        virtual int sh_mkdir(const char *path, mode_t mode) = 0;
        virtual int sh_unlink(const char *path) = 0;
        virtual int sh_rmdir(const char *path) = 0;
        virtual int sh_symlink(const char *from, const char *to) = 0;
        virtual int sh_rename(const char *from, const char *to) = 0;
        virtual int sh_link(const char *from, const char *to) = 0;
        virtual int sh_chmod(const char *path, mode_t mode) = 0;
        virtual int sh_chown(const char *path, uid_t uid, gid_t gid) = 0;
        virtual int sh_truncate(const char *path, off_t size) = 0;

        #ifdef HAVE_UTIMENSAT
        virtual int sh_utimens(const char *path, const struct timespec ts[2]) = 0;
        #endif // HAVE_UTIMENSAT

        virtual int sh_open(const char *path, struct fuse_file_info *fi) = 0;
        virtual int sh_read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fi) = 0;
        virtual int sh_write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fi) = 0;
        virtual int sh_statfs(const char *path, struct statvfs *stbuf) = 0;
        virtual int sh_release(const char *path, struct fuse_file_info *fi) = 0;
        virtual int sh_fsync(const char *path, int isdatasync, struct fuse_file_info *fi) = 0;

        #ifdef HAVE_POSIX_FALLOCATE
        virtual int sh_fallocate(const char *path, int mode, off_t offset, off_t length, struct fuse_file_info *fi) = 0;
        #endif // HAVE_POSIX_FALLOCATE

        /* xattr operations are optional and can safely be left unimplemented */
        #ifdef HAVE_SETXATTR
        virtual int sh_setxattr(const char *path, const char *name, const char *value, size_t size, int flags) = 0;
        virtual int sh_getxattr(const char *path, const char *name, char *value, size_t size) = 0;
        virtual int sh_listxattr(const char *path, char *list, size_t size) = 0;
        virtual int sh_removexattr(const char *path, const char *name) = 0;
        #endif // HAVE_SETXATTR
};

#endif // STORAGE_HELPER_I_HH
