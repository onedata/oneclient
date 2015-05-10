/**
* @file fsLogic.cc
* @author Rafal Slota
* @copyright (C) 2013 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "fsLogic.h"
#include "context.h"
#include "shMock.h"
#include "events/eventManager.h"

#include <glog/logging.h>
#include <boost/algorithm/string.hpp>

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
    m_shMock = std::make_unique<SHMock>("/tmp");
    m_eventManager = std::make_unique<events::EventManager>(m_context);
    LOG(INFO) << "Setting file system root directory to: '" << m_root << "'";
}

int FsLogic::access(const std::string &path, int mask)
{
    DLOG(INFO) << "FUSE: access(path: '" << path << "', mask: " << mask << ")";
    return m_shMock->shAccess(path, mask);
}

int FsLogic::getattr(
    const std::string &path, struct stat *statbuf, bool fuse_ctx)
{
    DLOG(INFO) << "FUSE: getattr(path: '" << path << "', fuse_ctx: " << fuse_ctx
               << ", ...)";
    return m_shMock->shGetattr(path, statbuf, fuse_ctx);
}

int FsLogic::readlink(const std::string &path, char *link, size_t size)
{
    DLOG(INFO) << "FUSE: readlink(path: '" << path << "', link: '" << link
               << "', size: " << size << ")";
    return m_shMock->shReadlink(path, link, size);
}

int FsLogic::mknod(const std::string &path, mode_t mode, dev_t dev)
{
    DLOG(INFO) << "FUSE: mknod(path: '" << path << "', mode: " << mode
               << ", dev: " << dev << ")";
    return m_shMock->shMknod(path, mode, dev);
}

int FsLogic::mkdir(const std::string &path, mode_t mode)
{
    DLOG(INFO) << "FUSE: mkdir(path: '" << path << "', mode: " << mode << ")";
    return m_shMock->shMkdir(path, mode);
}

int FsLogic::unlink(const std::string &path)
{
    DLOG(INFO) << "FUSE: unlink(path: '" << path << "')";
    return m_shMock->shUnlink(path);
}

int FsLogic::rmdir(const std::string &path)
{
    DLOG(INFO) << "FUSE: rmdir(path: '" << path << "')";
    return m_shMock->shRmdir(path);
}

int FsLogic::symlink(const std::string &path, const std::string &link)
{
    DLOG(INFO) << "FUSE: symlink(to: " << path << ", form: " << link << ")";
    return m_shMock->shSymlink(path, link);
}

int FsLogic::rename(const std::string &path, const std::string &newPath)
{
    DLOG(INFO) << "FUSE: rename(path: '" << path << "', newPath: '" << newPath
               << "')";
    return m_shMock->shRename(path, newPath);
}

int FsLogic::chmod(const std::string &path, mode_t mode)
{
    DLOG(INFO) << "FUSE: chmod(path: '" << path << "', mode: " << mode << ")";
    return m_shMock->shChmod(path, mode);
}

int FsLogic::chown(const std::string &path, uid_t uid, gid_t gid)
{
    DLOG(INFO) << "FUSE: chown(path: '" << path << "', uid: " << uid
               << ", gid: " << gid << ")";
    return m_shMock->shChown(path, uid, gid);
}

int FsLogic::truncate(const std::string &path, off_t newSize)
{
    DLOG(INFO) << "FUSE: truncate(path: '" << path << "', newSize: " << newSize
               << ")";
    int res = m_shMock->shTruncate(path, newSize);
    if (res == 0)
        m_eventManager->emitTruncateEvent(path, newSize);
    return res;
}

int FsLogic::utime(const std::string &path, struct utimbuf *ubuf)
{
    DLOG(INFO) << "FUSE: utime(path: '" << path << "', ...)";
    return m_shMock->shUtime(path, ubuf);
}

int FsLogic::open(const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: open(path: '" << path << "', ...)";
    return m_shMock->shOpen(path, fileInfo);
}

int FsLogic::read(const std::string &path, char *buf, size_t size, off_t offset,
    struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: read(path: '" << path << "', size: " << size
               << ", offset: " << offset << ", ...)";
    int res = m_shMock->shRead(path, buf, size, offset, fileInfo);
    if (res > 0)
        m_eventManager->emitReadEvent(path, offset, static_cast<size_t>(res));
    return res;
}

int FsLogic::write(const std::string &path, const char *buf, size_t size,
    off_t offset, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: write(path: '" << path << "', size: " << size
               << ", offset: " << offset << ", ...)";
    int res = m_shMock->shWrite(path, buf, size, offset, fileInfo);
    if (res > 0) {
        struct stat statbuf;
        if (m_shMock->shGetattr(path, &statbuf, true) == 0) {
            m_eventManager->emitWriteEvent(path, offset,
                static_cast<size_t>(res),
                std::max(offset + res, statbuf.st_size));
        }
        else {
            m_eventManager->emitWriteEvent(
                path, offset, static_cast<size_t>(res), offset + res);
        }
    }
    return res;
}

int FsLogic::statfs(const std::string &path, struct statvfs *statInfo)
{
    DLOG(INFO) << "FUSE: statfs(path: '" << path << "', ...)";
    return m_shMock->shStatfs(path, statInfo);
}

int FsLogic::flush(const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: flush(path: '" << path << "', ...)";
    return m_shMock->shFlush(path, fileInfo);
}

int FsLogic::release(const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: release(path: '" << path << "', ...)";
    return m_shMock->shRelease(path, fileInfo);
}

int FsLogic::fsync(
    const std::string &path, int datasync, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: fsync(path: '" << path << "', datasync: " << datasync
               << ", ...)";
    return m_shMock->shFsync(path, datasync, fileInfo);
}

int FsLogic::opendir(const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: opendir(path: '" << path << "', ...)";
    return m_shMock->shOpendir(path, fileInfo);
}

int FsLogic::readdir(const std::string &path, void *buf, fuse_fill_dir_t filler,
    off_t offset, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: readdir(path: '" << path
               << "', ..., offset: " << offset << ", ...)";
    return m_shMock->shReaddir(path, buf, filler, offset, fileInfo);
}

int FsLogic::releasedir(
    const std::string &path, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: releasedir(path: '" << path << "', ...)";
    return m_shMock->shReleasedir(path, fileInfo);
}

int FsLogic::fsyncdir(
    const std::string &path, int datasync, struct fuse_file_info *fileInfo)
{
    DLOG(INFO) << "FUSE: fsyncdir(path: '" << path
               << "', datasync: " << datasync << ", ...)";
    return m_shMock->shFsyncdir(path, datasync, fileInfo);
}

} // namespace client
} // namespace one
