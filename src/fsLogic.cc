/**
* @file fsLogic.cc
* @author Rafal Slota
* @copyright (C) 2013 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "fsLogic.h"
#include "context.h"

#include <glog/logging.h>
#include <boost/algorithm/string.hpp>

#include <unistd.h>

namespace one {
namespace client {

FsLogic::FsLogic(std::string path, std::shared_ptr<Context> context)
    : m_uid{geteuid()}
    , m_gid{getegid()}
    , m_context{std::move(context)}
{
    if (path.size() > 1 && path.back() == '/')
        path.pop_back();
    m_root = std::move(path);
    LOG(INFO) << "Setting file system root directory to: '" << m_root << "'";
}

int FsLogic::access(const std::string &path, int mask)
{
    DLOG(INFO) << "FUSE: access(path: '" << path << "', mask: " << mask << ")";
    return 0;
}

int FsLogic::getattr(const std::string &path, struct stat *statbuf,
                     bool fuse_ctx)
{
    DLOG(INFO) << "FUSE: getattr(path: '" << path << "', fuse_ctx: " << fuse_ctx
               << ", ...)";
    return 0;
}

int FsLogic::readlink(const std::string &path, char *link, size_t size)
{
    DLOG(INFO) << "FUSE: readlink(path: '" << path << "', link: '" << link
               << "', size: " << size << ")";
    return 0;
}

int FsLogic::mknod(const std::string &path, mode_t mode, dev_t dev)
{
    DLOG(INFO) << "FUSE: mknod(path: '" << path << "', mode: " << mode
               << ", dev: " << dev << ")";
    return 0;
}

int FsLogic::mkdir(const std::string &path, mode_t mode)
{
    DLOG(INFO) << "FUSE: mkdir(path: '" << path << "', mode: " << mode << ")";
    return 0;
}

int FsLogic::unlink(const std::string &path)
{
    DLOG(INFO) << "FUSE: unlink(path: '" << path << "')";
    return 0;
}

int FsLogic::rmdir(const std::string &path)
{
    DLOG(INFO) << "FUSE: rmdir(path: '" << path << "')";
    return 0;
}

int FsLogic::symlink(const std::string &to, const std::string &from)
{
    DLOG(INFO) << "FUSE: symlink(to: " << to << ", form: " << from << ")";
    return 0;
}

int FsLogic::rename(const std::string &path, const std::string &newPath)
{
    DLOG(INFO) << "FUSE: rename(path: '" << path << "', newPath: '" << newPath
               << "')";
    return 0;
}

int FsLogic::chmod(const std::string &path, mode_t mode)
{
    DLOG(INFO) << "FUSE: chmod(path: '" << path << "', mode: " << mode << ")";
    return 0;
}

int FsLogic::chown(const std::string &path, uid_t uid, gid_t gid)
{
    DLOG(INFO) << "FUSE: chown(path: '" << path << "', uid: " << uid
               << ", gid: " << gid << ")";
    return 0;
}

int FsLogic::truncate(const std::string &path, off_t newSize)
{
    DLOG(INFO) << "FUSE: truncate(path: '" << path << "', newSize: " << newSize
               << ")";
    return 0;
}

int FsLogic::utime(const std::string &path, struct utimbuf *ubuf)
{
    DLOG(INFO) << "FUSE: utime(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::open(const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: open(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::read(const std::string &path, char *buf, size_t size, off_t offset,
                  struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: read(path: '" << path << "', size: " << size
               << ", offset: " << offset << ", ...)";
    return 0;
}

int FsLogic::write(const std::string &path, const std::string &buf, size_t size,
                   off_t offset, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: write(path: '" << path << "', size: " << size
               << ", offset: " << offset << ", ...)";
    return 0;
}

int FsLogic::statfs(const std::string &path, struct statvfs *statInfo)
{
    DLOG(INFO) << "FUSE: statfs(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::flush(const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: flush(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::release(const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: release(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::fsync(const std::string &path, int datasync,
                   struct fuse_file_info *fuseFileInfo)
{
    DLOG(INFO) << "FUSE: fsync(path: '" << path << "', datasync: " << datasync
               << ", ...)";
    return 0;
}

int FsLogic::opendir(const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: opendir(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::readdir(const std::string &path, void *buf, fuse_fill_dir_t filler,
                     off_t offset, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: readdir(path: '" << path
               << "', ..., offset: " << offset << ", ...)";
    return 0;
}

int FsLogic::releasedir(const std::string &path,
                        struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: releasedir(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::fsyncdir(const std::string &path, int datasync,
                      struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: fsyncdir(path: '" << path
               << "', datasync: " << datasync << ", ...)";
    return 0;
}

} // namespace client
} // namespace one
