/**
 * @file directIOHelper.cc
 * @author Beata Skiba
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif // linux

#include "directIOHelper.h"

#include "helpers/storageHelperFactory.h"

#include <boost/any.hpp>

#include <dirent.h>
#include <errno.h>
#include <fuse.h>
#include <sys/stat.h>
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include <string>

using namespace std;

namespace one
{
namespace helpers
{

namespace
{
inline boost::filesystem::path extractPath(const IStorageHelper::ArgsMap &args)
{
    const auto arg = srvArg(0);
    return args.count(arg)
            ? boost::any_cast<std::string>(args.at(arg)).substr(0, PATH_MAX)
            : boost::filesystem::path{};
}
}

inline boost::filesystem::path DirectIOHelper::root(const boost::filesystem::path &path)
{
    return m_rootPath / path;
}

int DirectIOHelper::sh_getattr(const char *path, struct stat *stbuf)
{
    return lstat(root(path).c_str(), stbuf) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_access(const char *path, int mask)
{
    return access(root(path).c_str(), mask) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_readlink(const char *path, char *buf, size_t size)
{
    const int res = readlink(root(path).c_str(), buf, size - 1);

    if (res == -1)
        return -errno;

    buf[res] = '\0';
    return 0;
}


int DirectIOHelper::sh_readdir(const char *path, void *buf, fuse_fill_dir_t filler,
               off_t /*offset*/, struct fuse_file_info */*fi*/)
{
    DIR *dp = opendir(root(path).c_str());
    if (dp == nullptr)
        return -errno;


    for(struct dirent *de; (de = readdir(dp)) != nullptr;)
    {
        struct stat st;
        memset(&st, 0, sizeof(st));
        st.st_ino = de->d_ino;
        st.st_mode = de->d_type << 12;
        if (filler(buf, de->d_name, &st, 0))
            break;
    }

    closedir(dp);
    return 0;
}

int DirectIOHelper::sh_mknod(const char *path, mode_t mode, dev_t rdev)
{
    int res;
    const auto fullPath = root(path);

    /* On Linux this could just be 'mknod(path, mode, rdev)' but this
       is more portable */
    if (S_ISREG(mode)) {
        res = open(fullPath.c_str(), O_CREAT | O_EXCL | O_WRONLY, mode);
        if (res >= 0)
            res = close(res);
    } else if (S_ISFIFO(mode))
        res = mkfifo(fullPath.c_str(), mode);
    else
        res = mknod(fullPath.c_str(), mode, rdev);

    if (res == -1)
        return -errno;

    return 0;
}

int DirectIOHelper::sh_mkdir(const char *path, mode_t mode)
{
    return mkdir(root(path).c_str(), mode) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_unlink(const char *path)
{
    return unlink(root(path).c_str()) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_rmdir(const char *path)
{
    return rmdir(root(path).c_str()) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_symlink(const char *from, const char *to)
{
    return symlink(root(from).c_str(), root(to).c_str()) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_rename(const char *from, const char *to)
{
    return rename(root(from).c_str(), root(to).c_str()) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_link(const char *from, const char *to)
{
    return link(root(from).c_str(), root(to).c_str()) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_chmod(const char *path, mode_t mode)
{
    return chmod(root(path).c_str(), mode) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_chown(const char *path, uid_t uid, gid_t gid)
{
    return lchown(root(path).c_str(), uid, gid) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_truncate(const char *path, off_t size)
{
    return truncate(root(path).c_str(), size) == -1 ? -errno : 0;
}

#ifdef HAVE_UTIMENSAT
int DirectIOHelper::sh_utimens(const char *path, const struct timespec ts[2])
{
    /* don't use utime/utimes since they follow symlinks */
    return utimensat(0, root(path).c_str(), ts, AT_SYMLINK_NOFOLLOW) == -1 ? -errno : 0;
}
#endif /* HAVE_UTIMENSAT */

int DirectIOHelper::sh_open(const char *path, struct fuse_file_info *fi)
{
    const int res = open(root(path).c_str(), fi->flags);
    if (res == -1)
        return -errno;

    fi->fh = res;
    return 0;
}

int DirectIOHelper::sh_read(const char *path, char *buf, size_t size, off_t offset,
            struct fuse_file_info *fi)
{
    int fd;
    int res;
    if(fi->fh > 0)
        fd = fi->fh;
    else
        fd = open(root(path).c_str(), O_RDONLY);

    if (fd == -1)
        return -errno;

    res = pread(fd, buf, size, offset);
    if (res == -1)
        res = -errno;

    if(fi->fh <= 0)
        close(fd);

    return res;
}

int DirectIOHelper::sh_write(const char *path, const char *buf, size_t size,
             off_t offset, struct fuse_file_info *fi)
{
    int fd;
    int res;
    if(fi->fh > 0)
        fd = fi->fh;
    else
       fd = open(root(path).c_str(), O_WRONLY);

    if (fd == -1)
        return -errno;

    res = pwrite(fd, buf, size, offset);
    if (res == -1)
        res = -errno;

    if(fi->fh <= 0)
        close(fd);

    return res;
}

int DirectIOHelper::sh_statfs(const char *path, struct statvfs *stbuf)
{
    return statvfs(root(path).c_str(), stbuf) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_release(const char */*path*/, struct fuse_file_info *fi)
{
    return fi->fh > 0 ? close(fi->fh) : 0;
}

int DirectIOHelper::sh_fsync(const char */*path*/, int /*isdatasync*/,
             struct fuse_file_info */*fi*/)
{
    /* Just a stub.     This method is optional and can safely be left
       unimplemented */

    return 0;
}

int DirectIOHelper::sh_flush(const char */*path*/, struct fuse_file_info */*fi*/)
{
    return 0;
}

#ifdef HAVE_POSIX_FALLOCATE
int DirectIOHelper::sh_fallocate(const char *path, int mode,
            off_t offset, off_t length, struct fuse_file_info *fi)
{
    int fd;
    int res;

    (void) fi;

    if (mode)
        return -EOPNOTSUPP;

    fd = open(root(path).c_str(), O_WRONLY);

    if (fd == -1)
        return -errno;

    res = -posix_fallocate(fd, offset, length);

    close(fd);
    return res;
}
#endif  /* HAVE_POSIX_FALLOCATE */

#ifdef HAVE_SETXATTR
/* xattr operations are optional and can safely be left unimplemented */
int DirectIOHelper::sh_setxattr(const char *path, const char *name,
                                const char *value, size_t size, int flags)
{
    return lsetxattr(root(path).c_str(), name, value, size, flags) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_getxattr(const char *path, const char *name, char *value,
            size_t size)
{
    return lgetxattr(root(path).c_str(), name, value, size) == -1 ? -errno : 0;
}

int DirectIOHelper::sh_listxattr(const char *path, char *list, size_t size)
{
    return llistxattr(root(path).c_str(), list, size)  == -1 ? -errno : 0;
}

int DirectIOHelper::sh_removexattr(const char *path, const char *name)
{
    return lremovexattr(root(path).c_str(), name) == -1 ? -errno : 0;
}

#endif /* HAVE_SETXATTR */

DirectIOHelper::DirectIOHelper(const ArgsMap &args)
    : m_rootPath{extractPath(args)}
{
}


} // namespace helpers
} // namespace one
