/**
 * @file fsLogic.cc
 * @author Rafal Slota
 * @author Konrad Zemek
 * @copyright (C) 2013-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsLogic.h"

#include "context.h"
#include "events/eventManager.h"
#include "logging.h"
#include "shMock.h"
#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/fileAttr.h"

#include <boost/algorithm/string.hpp>

using namespace std::literals;

namespace one {
namespace client {

FsLogic::FsLogic(std::string root, std::shared_ptr<Context> context)
    : m_root{std::move(root)}
    , m_uid{geteuid()}
    , m_gid{getegid()}
    , m_context{std::move(context)}
    , m_shMock{std::make_unique<SHMock>("/tmp")}
    , m_eventManager{std::make_unique<events::EventManager>(m_context)}
{
    if (m_root.size() > 1 && m_root.back() == '/')
        m_root.pop_back();

    LOG(INFO) << "Setting file system root directory to: '" << m_root << "'";
}

int FsLogic::access(const std::string &path, const int mask)
{
    DLOG(INFO) << "FUSE: access(path: '" << path << "', mask: " << mask << ")";
    return m_shMock->shAccess(path, mask);
}

static void setattrs(struct stat *const statbuf, const messages::fuse::FileAttr &attr)
{
    statbuf->st_atime = std::chrono::system_clock::to_time_t(attr.atime());
    statbuf->st_mtime = std::chrono::system_clock::to_time_t(attr.mtime());
    statbuf->st_ctime = std::chrono::system_clock::to_time_t(attr.ctime());
    statbuf->st_gid = attr.gid();
    statbuf->st_uid = attr.uid();
    statbuf->st_mode = attr.mode();
    statbuf->st_size = attr.size();
}

int FsLogic::getattr(const std::string &path, struct stat *const statbuf)
{
    DLOG(INFO) << "FUSE: getattr(path: '" << path << ", ...)";

    messages::fuse::GetFileAttr request;

    decltype(m_uuidCache)::const_accessor acc;
    if (m_uuidCache.find(acc, path)) {
        auto uuid = acc->second;
        acc.release();

        decltype(m_attrCache)::const_accessor attrAcc;
        if (m_attrCache.find(attrAcc, uuid)) {
            setattrs(statbuf, attrAcc->second);
            return 0;
        }

        request.setUUID(std::move(uuid));
    }
    else
        request.setPath(path);

    auto future =
        m_context->communicator()->communicate<messages::fuse::FileAttr>(
            request);

    try {
        auto attr = future.get(10s);
        setattrs(statbuf, attr);

        m_uuidCache.insert({path, attr.uuid()});
        m_attrCache.insert({attr.uuid(), std::move(attr)});

        return 0;
    }
    catch (const std::system_error &e) {
        return -1 * e.code().value();
    }
    catch (const communication::TimeoutExceeded &t) {
        return -1 * std::make_error_code(std::errc::timed_out).value();
    }
    catch (const communication::Exception &t) {
        return -1 * std::make_error_code(std::errc::io_error).value();
    }
}

int FsLogic::readlink(const std::string &path, boost::asio::mutable_buffer buf)
{
    DLOG(INFO) << "FUSE: readlink(path: '" << path
               << "', bufferSize: " << boost::asio::buffer_size(buf) << ")";

    return m_shMock->shReadlink(path, buf);
}

int FsLogic::mknod(const std::string &path, const mode_t mode, const dev_t dev)
{
    DLOG(INFO) << "FUSE: mknod(path: '" << path << "', mode: " << mode
               << ", dev: " << dev << ")";
    return m_shMock->shMknod(path, mode, dev);
}

int FsLogic::mkdir(const std::string &path, const mode_t mode)
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

int FsLogic::symlink(const std::string &target, const std::string &linkPath)
{
    DLOG(INFO) << "FUSE: symlink(target: " << target
               << ", linkPath: " << linkPath << ")";

    return m_shMock->shSymlink(target, linkPath);
}

int FsLogic::rename(const std::string &oldPath, const std::string &newPath)
{
    DLOG(INFO) << "FUSE: rename(oldPath: '" << oldPath << "', newPath: '"
               << newPath << "')";

    return m_shMock->shRename(oldPath, newPath);
}

int FsLogic::chmod(const std::string &path, const mode_t mode)
{
    DLOG(INFO) << "FUSE: chmod(path: '" << path << "', mode: " << mode << ")";
    return m_shMock->shChmod(path, mode);
}

int FsLogic::chown(const std::string &path, const uid_t uid, const gid_t gid)
{
    DLOG(INFO) << "FUSE: chown(path: '" << path << "', uid: " << uid
               << ", gid: " << gid << ")";

    return m_shMock->shChown(path, uid, gid);
}

int FsLogic::truncate(const std::string &path, const off_t newSize)
{
    DLOG(INFO) << "FUSE: truncate(path: '" << path << "', newSize: " << newSize
               << ")";

    int res = m_shMock->shTruncate(path, newSize);
    if (res == 0)
        m_eventManager->emitTruncateEvent(path, newSize);
    return res;
}

int FsLogic::utime(const std::string &path, struct utimbuf *const ubuf)
{
    DLOG(INFO) << "FUSE: utime(path: '" << path << "', ...)";
    return m_shMock->shUtime(path, ubuf);
}

int FsLogic::open(
    const std::string &path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: open(path: '" << path << "', ...)";
    return m_shMock->shOpen(path, fileInfo);
}

int FsLogic::read(const std::string &path, boost::asio::mutable_buffer buf,
    const off_t offset, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: read(path: '" << path
               << "', bufferSize: " << boost::asio::buffer_size(buf)
               << ", offset: " << offset << ", ...)";

    int res = m_shMock->shRead(path, buf, offset, fileInfo);
    if (res > 0)
        m_eventManager->emitReadEvent(path, offset, static_cast<size_t>(res));

    return res;
}

int FsLogic::write(const std::string &path, boost::asio::const_buffer buf,
    const off_t offset, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: write(path: '" << path
               << "', bufferSize: " << boost::asio::buffer_size(buf)
               << ", offset: " << offset << ", ...)";

    int res = m_shMock->shWrite(path, buf, offset, fileInfo);
    if (res > 0) {
        struct stat statbuf;
        if (m_shMock->shGetattr(path, &statbuf) == 0) {
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

int FsLogic::statfs(const std::string &path, struct statvfs *const statInfo)
{
    DLOG(INFO) << "FUSE: statfs(path: '" << path << "', ...)";
    return m_shMock->shStatfs(path, statInfo);
}

int FsLogic::flush(
    const std::string &path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: flush(path: '" << path << "', ...)";
    return m_shMock->shFlush(path, fileInfo);
}

int FsLogic::release(
    const std::string &path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: release(path: '" << path << "', ...)";
    return m_shMock->shRelease(path, fileInfo);
}

int FsLogic::fsync(const std::string &path, const int datasync,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: fsync(path: '" << path << "', datasync: " << datasync
               << ", ...)";

    return m_shMock->shFsync(path, datasync, fileInfo);
}

int FsLogic::opendir(
    const std::string &path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: opendir(path: '" << path << "', ...)";
    return m_shMock->shOpendir(path, fileInfo);
}

int FsLogic::readdir(const std::string &path, void *const buf,
    const fuse_fill_dir_t filler, const off_t offset,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: readdir(path: '" << path
               << "', ..., offset: " << offset << ", ...)";

    return m_shMock->shReaddir(path, buf, filler, offset, fileInfo);
}

int FsLogic::releasedir(
    const std::string &path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: releasedir(path: '" << path << "', ...)";
    return m_shMock->shReleasedir(path, fileInfo);
}

int FsLogic::fsyncdir(const std::string &path, const int datasync,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: fsyncdir(path: '" << path
               << "', datasync: " << datasync << ", ...)";

    return m_shMock->shFsyncdir(path, datasync, fileInfo);
}

} // namespace client
} // namespace one
