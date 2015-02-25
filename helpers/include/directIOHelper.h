/**
 * @file directIOHelper.h
 * @author Beata Skiba
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef HELPERS_DIRECT_IO_HELPER_H
#define HELPERS_DIRECT_IO_HELPER_H


#include "helpers/IStorageHelper.h"

#include <boost/filesystem/path.hpp>

#include <fuse.h>
#include <sys/types.h>

namespace one
{
namespace helpers
{

/**
 * The DirectIOHelper class
 * Storage helper used to access files on mounted as local filesystem.
 */
class DirectIOHelper: public IStorageHelper
{
public:
    /**
     * This storage helper uses only the first element of args map.
     * It shall be ablosute path to diretory used by this storage helper as
     * root mount point.
     */
    DirectIOHelper(const ArgsMap&);

    int sh_getattr(const char *path, struct stat *stbuf) ;
    int sh_access(const char *path, int mask) ;
    int sh_readlink(const char *path, char *buf, size_t size) ;
    int sh_readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fi) ;
    int sh_mknod(const char *path, mode_t mode, dev_t rdev) ;
    int sh_mkdir(const char *path, mode_t mode) ;
    int sh_unlink(const char *path) ;
    int sh_rmdir(const char *path) ;
    int sh_symlink(const char *from, const char *to) ;
    int sh_rename(const char *from, const char *to) ;
    int sh_link(const char *from, const char *to) ;
    int sh_chmod(const char *path, mode_t mode) ;
    int sh_chown(const char *path, uid_t uid, gid_t gid) ;
    int sh_truncate(const char *path, off_t size) ;

    #ifdef HAVE_UTIMENSAT
    int sh_utimens(const char *path, const struct timespec ts[2]);
    #endif // HAVE_UTIMENSAT

    int sh_open(const char *path, struct fuse_file_info *fi) ;
    int sh_read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fi) ;
    int sh_write(const char *path, const char *buf, size_t size, off_t offset, struct fuse_file_info *fi) ;
    int sh_statfs(const char *path, struct statvfs *stbuf) ;
    int sh_flush(const char *path, struct fuse_file_info *fi) ;
    int sh_release(const char *path, struct fuse_file_info *fi) ;
    int sh_fsync(const char *path, int isdatasync, struct fuse_file_info *fi);

    #ifdef HAVE_POSIX_FALLOCATE
    int sh_fallocate(const char *path, int mode, off_t offset, off_t length, struct fuse_file_info *fi) ;
    #endif // HAVE_POSIX_FALLOCATE

    /* xattr operations are optional and can safely be left unimplemented */
    #ifdef HAVE_SETXATTR
    int sh_setxattr(const char *path, const char *name, const char *value, size_t size, int flags) ;
    int sh_getxattr(const char *path, const char *name, char *value, size_t size) ;
    int sh_listxattr(const char *path, char *list, size_t size) ;
    int sh_removexattr(const char *path, const char *name) ;
    #endif // HAVE_SETXATTR

private:
    boost::filesystem::path root(const boost::filesystem::path &path);

    const boost::filesystem::path m_rootPath;
};

} // namespace helpers
} // namespace one


#endif // HELPERS_DIRECT_IO_HELPER_H
