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
#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/createDir.h"
#include "messages/fuse/deleteFile.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/getFileChildren.h"
#include "messages/fuse/updateTimes.h"

#include <boost/algorithm/string.hpp>

using namespace std::literals;

namespace one {
namespace client {

FsLogic::FsLogic(boost::filesystem::path root, std::shared_ptr<Context> context)
    : m_root{std::move(root)}
    , m_uid{geteuid()}
    , m_gid{getegid()}
    , m_context{std::move(context)}
    , m_eventManager{std::make_unique<events::EventManager>(m_context)}
{
    LOG(INFO) << "Setting file system root directory to: '" << m_root << "'";
}

int FsLogic::access(boost::filesystem::path path, const int mask)
{
    DLOG(INFO) << "FUSE: access(path: '" << path << "', mask: " << mask << ")";
    return 0;
}

int FsLogic::getattr(boost::filesystem::path path, struct stat *const statbuf)
{
    DLOG(INFO) << "FUSE: getattr(path: '" << path << ", ...)";

    auto attr = getAttr(path);
    statbuf->st_atime = std::chrono::system_clock::to_time_t(attr.atime());
    statbuf->st_mtime = std::chrono::system_clock::to_time_t(attr.mtime());
    statbuf->st_ctime = std::chrono::system_clock::to_time_t(attr.ctime());
    statbuf->st_gid = attr.gid();
    statbuf->st_uid = attr.uid();
    statbuf->st_mode = attr.mode();
    statbuf->st_size = attr.size();
    return 0;
}

int FsLogic::readlink(
    boost::filesystem::path path, boost::asio::mutable_buffer buf)
{
    DLOG(INFO) << "FUSE: readlink(path: '" << path
               << "', bufferSize: " << boost::asio::buffer_size(buf) << ")";

    throw std::errc::operation_not_supported;
}

int FsLogic::mknod(
    boost::filesystem::path path, const mode_t mode, const dev_t dev)
{
    DLOG(INFO) << "FUSE: mknod(path: '" << path << "', mode: " << mode
               << ", dev: " << dev << ")";

    throw std::errc::operation_not_supported;
}

int FsLogic::mkdir(boost::filesystem::path path, const mode_t mode)
{
    DLOG(INFO) << "FUSE: mkdir(path: '" << path << "', mode: " << mode << ")";

    auto parentAttr = getAttr(path.parent_path());
    if (parentAttr.type() != messages::fuse::FileAttr::FileType::directory)
        throw std::errc::not_a_directory;

    messages::fuse::CreateDir msg{
        parentAttr.uuid(), path.filename().string(), mode};

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            msg);

    communication::wait(future);

    return 0;
}

int FsLogic::unlink(boost::filesystem::path path)
{
    DLOG(INFO) << "FUSE: unlink(path: '" << path << "')";
    removeFile(std::move(path));
    return 0;
}

int FsLogic::rmdir(boost::filesystem::path path)
{
    DLOG(INFO) << "FUSE: rmdir(path: '" << path << "')";
    removeFile(std::move(path));
    return 0;
}

int FsLogic::symlink(
    boost::filesystem::path target, boost::filesystem::path linkPath)
{
    DLOG(INFO) << "FUSE: symlink(target: " << target
               << ", linkPath: " << linkPath << ")";

    throw std::errc::operation_not_supported;
}

int FsLogic::rename(
    boost::filesystem::path oldPath, boost::filesystem::path newPath)
{
    DLOG(INFO) << "FUSE: rename(oldPath: '" << oldPath << "', newPath: '"
               << newPath << "')";

    throw std::errc::operation_not_supported;
}

int FsLogic::chmod(boost::filesystem::path path, const mode_t mode)
{
    DLOG(INFO) << "FUSE: chmod(path: '" << path << "', mode: " << mode << ")";
    throw std::errc::operation_not_supported;
}

int FsLogic::chown(
    boost::filesystem::path path, const uid_t uid, const gid_t gid)
{
    DLOG(INFO) << "FUSE: chown(path: '" << path << "', uid: " << uid
               << ", gid: " << gid << ")";

    throw std::errc::operation_not_supported;
}

int FsLogic::truncate(boost::filesystem::path path, const off_t newSize)
{
    DLOG(INFO) << "FUSE: truncate(path: '" << path << "', newSize: " << newSize
               << ")";

    //    m_eventManager->emitTruncateEvent(path.string(), newSize);
    throw std::errc::operation_not_supported;
}

int FsLogic::utime(boost::filesystem::path path, struct utimbuf *const ubuf)
{
    DLOG(INFO) << "FUSE: utime(path: '" << path << "', ...)";

    auto attr = getAttr(path);
    messages::fuse::UpdateTimes msg{attr.uuid()};

    if (!ubuf) {
        const auto now = std::chrono::system_clock::now();
        msg.atime(now);
        msg.mtime(now);
    }
    else {
        msg.atime(std::chrono::system_clock::from_time_t(ubuf->actime));
        msg.mtime(std::chrono::system_clock::from_time_t(ubuf->modtime));
    }

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            msg);

    communication::wait(future);

    return 0;
}

int FsLogic::open(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: open(path: '" << path << "', ...)";
    throw std::errc::operation_not_supported;
}

int FsLogic::read(boost::filesystem::path path, boost::asio::mutable_buffer buf,
    const off_t offset, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: read(path: '" << path
               << "', bufferSize: " << boost::asio::buffer_size(buf)
               << ", offset: " << offset << ", ...)";

    //    m_eventManager->emitReadEvent(
    //        path.string(), offset, static_cast<size_t>(res));

    throw std::errc::operation_not_supported;
}

int FsLogic::write(boost::filesystem::path path, boost::asio::const_buffer buf,
    const off_t offset, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: write(path: '" << path
               << "', bufferSize: " << boost::asio::buffer_size(buf)
               << ", offset: " << offset << ", ...)";

    //    if (m_shMock->shGetattr(path.string(), &statbuf) == 0) {
    //        m_eventManager->emitWriteEvent(path.string(), offset,
    //            static_cast<size_t>(res),
    //            std::max(offset + res, statbuf.st_size));
    //    }
    //    else {
    //        m_eventManager->emitWriteEvent(
    //            path.string(), offset, static_cast<size_t>(res), offset +
    //            res);
    //    }

    throw std::errc::operation_not_supported;
}

int FsLogic::statfs(
    boost::filesystem::path path, struct statvfs *const statInfo)
{
    DLOG(INFO) << "FUSE: statfs(path: '" << path << "', ...)";
    throw std::errc::operation_not_supported;
}

int FsLogic::flush(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: flush(path: '" << path << "', ...)";
    throw std::errc::operation_not_supported;
}

int FsLogic::release(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: release(path: '" << path << "', ...)";
    throw std::errc::operation_not_supported;
}

int FsLogic::fsync(boost::filesystem::path path, const int datasync,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: fsync(path: '" << path << "', datasync: " << datasync
               << ", ...)";

    throw std::errc::operation_not_supported;
}

int FsLogic::opendir(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: opendir(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::readdir(boost::filesystem::path path, void *const buf,
    const fuse_fill_dir_t filler, const off_t offset,
    struct fuse_file_info *const /*fileInfo*/)
{
    DLOG(INFO) << "FUSE: readdir(path: '" << path
               << "', ..., offset: " << offset << ", ...)";

    auto attr = getAttr(path);
    if (attr.type() != messages::fuse::FileAttr::FileType::directory)
        throw std::errc::not_a_directory;

    messages::fuse::GetFileChildren msg{attr.uuid(), offset, 1000};
    auto future =
        m_context->communicator()->communicate<messages::fuse::FileChildren>(
            msg);

    auto fileChildren = communication::wait(future);
    auto currentOffset = offset;

    for (const auto &uuidAndName : fileChildren.uuidsAndNames()) {
        auto name = std::get<1>(uuidAndName);
        auto childPath = path / name;
        m_uuidCache.insert({std::move(childPath), std::get<0>(uuidAndName)});

        if (filler(buf, name.c_str(), nullptr, ++currentOffset))
            break;
    }

    return 0;
}

int FsLogic::releasedir(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: releasedir(path: '" << path << "', ...)";
    return 0;
}

int FsLogic::fsyncdir(boost::filesystem::path path, const int datasync,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: fsyncdir(path: '" << path
               << "', datasync: " << datasync << ", ...)";

    return 0;
}

void FsLogic::removeFile(boost::filesystem::path path)
{
    auto attr = getAttr(path);
    messages::fuse::DeleteFile msg{attr.uuid()};

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            msg);

    communication::wait(future);

    m_uuidCache.erase(path);
    m_attrCache.erase(attr.uuid());
}

messages::fuse::FileAttr FsLogic::getAttr(const boost::filesystem::path &path)
{
    decltype(m_uuidCache)::const_accessor acc;
    if (m_uuidCache.find(acc, path)) {
        auto uuid = acc->second;
        acc.release();

        return getAttr(uuid);
    }

    return getAttr(messages::fuse::GetFileAttr{path});
}

messages::fuse::FileAttr FsLogic::getAttr(const std::string &uuid)
{
    decltype(m_attrCache)::const_accessor acc;
    if (m_attrCache.find(acc, uuid))
        return acc->second;

    return getAttr(messages::fuse::GetFileAttr{uuid});
}

messages::fuse::FileAttr FsLogic::getAttr(
    const messages::fuse::GetFileAttr &req)
{
    auto future =
        m_context->communicator()->communicate<messages::fuse::FileAttr>(req);

    auto attr = communication::wait(future);
    m_attrCache.insert({attr.uuid(), attr});

    return attr;
}

std::size_t FsLogic::PathHash::hash(const boost::filesystem::path &path)
{
    return std::hash<std::string>{}(path.string());
}

bool FsLogic::PathHash::equal(
    const boost::filesystem::path &a, const boost::filesystem::path &b)
{
    return a == b;
}

} // namespace client
} // namespace one
