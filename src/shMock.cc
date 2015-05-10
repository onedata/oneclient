/**
* @file shMock.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif // linux

#include "shMock.h"
#include "logging.h"

#include <fuse.h>
#include <errno.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <iostream>

namespace one {
namespace client {

SHMock::SHMock(std::string root)
    : m_root{std::move(root)}
{
}

boost::filesystem::path SHMock::root(const boost::filesystem::path &path)
{
    return m_root / path;
}

int SHMock::shAccess(const std::string &path, int mask)
{
    return access(root(path).c_str(), mask) == -1 ? -errno : 0;
}

int SHMock::shGetattr(
    const std::string &path, struct stat *statbuf, bool fuse_ctx)
{
    return lstat(root(path).c_str(), statbuf) == -1 ? -errno : 0;
}

int SHMock::shReadlink(const std::string &path, char *link, size_t size)
{
    int res = readlink(root(path).c_str(), link, size - 1);

    if (res == -1)
        return -errno;

    link[res] = '\0';
    return 0;
}

int SHMock::shMknod(const std::string &path, mode_t mode, dev_t dev)
{
    int res;
    const auto fullPath = root(path);

    if (S_ISREG(mode)) {
        res = open(fullPath.c_str(), O_CREAT | O_EXCL | O_WRONLY, mode);
        if (res >= 0)
            res = close(res);
    }
    else if (S_ISFIFO(mode))
        res = mkfifo(fullPath.c_str(), mode);
    else
        res = mknod(fullPath.c_str(), mode, dev);

    if (res == -1)
        return -errno;

    return 0;
}

int SHMock::shMkdir(const std::string &path, mode_t mode)
{
    return mkdir(root(path).c_str(), mode) == -1 ? -errno : 0;
}

int SHMock::shUnlink(const std::string &path)
{
    return unlink(root(path).c_str()) == -1 ? -errno : 0;
}

int SHMock::shRmdir(const std::string &path)
{
    return rmdir(root(path).c_str()) == -1 ? -errno : 0;
}

int SHMock::shSymlink(const std::string &path, const std::string &link)
{
    return symlink(root(link).c_str(), root(path).c_str()) == -1 ? -errno : 0;
}

int SHMock::shRename(const std::string &path, const std::string &newPath)
{
    return rename(root(path).c_str(), root(newPath).c_str()) == -1 ? -errno : 0;
}

int SHMock::shChmod(const std::string &path, mode_t mode)
{
    return chmod(root(path).c_str(), mode) == -1 ? -errno : 0;
}

int SHMock::shChown(const std::string &path, uid_t uid, gid_t gid)
{
    return lchown(root(path).c_str(), uid, gid) == -1 ? -errno : 0;
}

int SHMock::shTruncate(const std::string &path, off_t newSize)
{
    return truncate(root(path).c_str(), newSize) == -1 ? -errno : 0;
}

int SHMock::shUtime(const std::string &path, struct utimbuf *ubuf)
{
    return utime(root(path).c_str(), ubuf) == -1 ? -errno : 0;
}

int SHMock::shOpen(const std::string &path, struct fuse_file_info *fileInfo)
{
    int res = open(root(path).c_str(), fileInfo->flags);
    if (res == -1)
        return -errno;

    fileInfo->fh = res;
    return 0;
}

int SHMock::shRead(const std::string &path, char *buf, size_t size,
    off_t offset, struct fuse_file_info *fileInfo)
{
    int fd, res;
    if (fileInfo->fh > 0)
        fd = fileInfo->fh;
    else
        fd = open(root(path).c_str(), O_RDONLY);

    if (fd == -1)
        return -errno;

    res = pread(fd, buf, size, offset);
    if (res == -1)
        res = -errno;

    if (fileInfo->fh <= 0)
        close(fd);

    return res;
}

int SHMock::shWrite(const std::string &path, const char *buf, size_t size,
    off_t offset, struct fuse_file_info *fileInfo)
{
    int fd, res;
    if (fileInfo->fh > 0)
        fd = fileInfo->fh;
    else
        fd = open(root(path).c_str(), O_WRONLY);

    if (fd == -1)
        return -errno;

    res = pwrite(fd, buf, size, offset);
    if (res == -1)
        res = -errno;

    if (fileInfo->fh <= 0)
        close(fd);

    return res;
}

int SHMock::shStatfs(const std::string &path, struct statvfs *statInfo)
{
    return statvfs(root(path).c_str(), statInfo) == -1 ? -errno : 0;
}

int SHMock::shFlush(const std::string &path, struct fuse_file_info *fileInfo)
{
    return 0;
}

int SHMock::shRelease(const std::string &path, struct fuse_file_info *fileInfo)
{
    return fileInfo->fh > 0 ? close(fileInfo->fh) : 0;
}

int SHMock::shFsync(
    const std::string &path, int datasync, struct fuse_file_info *fileInfo)
{
    return 0;
}

int SHMock::shOpendir(const std::string &path, struct fuse_file_info *fileInfo)
{
    return 0;
}

int SHMock::shReaddir(const std::string &path, void *buf,
    fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fileInfo)
{
    DIR *dp = opendir(root(path).c_str());
    if (dp == nullptr)
        return -errno;

    for (struct dirent *de; (de = readdir(dp)) != nullptr;) {
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

int SHMock::shReleasedir(
    const std::string &path, struct fuse_file_info *fileInfo)
{
    return 0;
}

int SHMock::shFsyncdir(
    const std::string &path, int datasync, struct fuse_file_info *fileInfo)
{
    return 0;
}

} // namespace client
} // namespace one
