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
#include "helperWrapper.h"

#include "messages/fuse/changeMode.h"
#include "messages/fuse/close.h"
#include "messages/fuse/createDir.h"
#include "messages/fuse/deleteFile.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileLocation.h"
#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/getFileChildren.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/getHelperParams.h"
#include "messages/fuse/getNewFileLocation.h"
#include "messages/fuse/helperParams.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/truncate.h"
#include "messages/fuse/updateTimes.h"

#include <boost/algorithm/string.hpp>

#include <algorithm>

using namespace std::literals;

namespace one {
namespace client {

FsLogic::FsLogic(std::shared_ptr<Context> context)
    : m_uid{geteuid()}
    , m_gid{getegid()}
    , m_context{std::move(context)}
    , m_eventManager{std::make_unique<events::EventManager>(m_context)}
    , m_helpersCache{*m_context->communicator()}
    , m_metadataCache{*m_context->communicator()}
    , m_pushListener{*m_context->communicator(), m_metadataCache}
{
}

int FsLogic::access(boost::filesystem::path path, const int mask)
{
    DLOG(INFO) << "FUSE: access(path: " << path << ", mask: " << mask << ")";
    return 0;
}

int FsLogic::getattr(boost::filesystem::path path, struct stat *const statbuf)
{
    DLOG(INFO) << "FUSE: getattr(path: " << path << ", ...)";

    auto attr = m_metadataCache.getAttr(path);

    statbuf->st_atime = std::chrono::system_clock::to_time_t(attr.atime());
    statbuf->st_mtime = std::chrono::system_clock::to_time_t(attr.mtime());
    statbuf->st_ctime = std::chrono::system_clock::to_time_t(attr.ctime());
    statbuf->st_gid = attr.gid();
    statbuf->st_uid = attr.uid();
    statbuf->st_mode = attr.mode();
    statbuf->st_size = attr.size();
    statbuf->st_nlink = 1;
    statbuf->st_blocks = 0;

    switch (attr.type()) {
        case messages::fuse::FileAttr::FileType::directory:
            statbuf->st_mode |= S_IFDIR;
            break;
        case messages::fuse::FileAttr::FileType::link:
            statbuf->st_mode |= S_IFLNK;
            break;
        case messages::fuse::FileAttr::FileType::regular:
            statbuf->st_mode |= S_IFREG;
            break;
    }

    return 0;
}

int FsLogic::readlink(boost::filesystem::path path, asio::mutable_buffer buf)
{
    DLOG(INFO) << "FUSE: readlink(path: " << path
               << ", bufferSize: " << asio::buffer_size(buf) << ")";

    throw std::errc::operation_not_supported;
}

int FsLogic::mknod(
    boost::filesystem::path path, const mode_t mode, const dev_t dev)
{
    DLOG(INFO) << "FUSE: mknod(path: " << path << ", mode: " << std::oct << mode
               << ", dev: " << dev << ")";

    auto parentAttr = m_metadataCache.getAttr(path.parent_path());
    if (parentAttr.type() != messages::fuse::FileAttr::FileType::directory)
        throw std::errc::not_a_directory;

    messages::fuse::GetNewFileLocation msg{
        path.filename().string(), parentAttr.uuid(), mode};

    auto future =
        m_context->communicator()->communicate<messages::fuse::FileLocation>(
            std::move(msg));

    auto location = communication::wait(future);
    m_metadataCache.map(path, location);

    auto helper = getHelper(location.storageId());
    HelperWrapper(*helper).mknod({location.fileId()}, mode, dev);
    return 0;
}

int FsLogic::mkdir(boost::filesystem::path path, const mode_t mode)
{
    DLOG(INFO) << "FUSE: mkdir(path: " << path << ", mode: " << std::oct << mode
               << ")";

    auto parentAttr = m_metadataCache.getAttr(path.parent_path());
    if (parentAttr.type() != messages::fuse::FileAttr::FileType::directory)
        throw std::errc::not_a_directory;

    messages::fuse::CreateDir msg{
        parentAttr.uuid(), path.filename().string(), mode};

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            std::move(msg));

    communication::wait(future);

    return 0;
}

int FsLogic::unlink(boost::filesystem::path path)
{
    DLOG(INFO) << "FUSE: unlink(path: " << path << "')";
    removeFile(std::move(path));
    return 0;
}

int FsLogic::rmdir(boost::filesystem::path path)
{
    DLOG(INFO) << "FUSE: rmdir(path: " << path << "')";
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
    DLOG(INFO) << "FUSE: rename(oldpath: " << oldPath
               << ", newpath: " << newPath << "')";

    m_metadataCache.rename(oldPath, newPath);

    return 0;
}

int FsLogic::chmod(boost::filesystem::path path, const mode_t mode)
{
    DLOG(INFO) << "FUSE: chmod(path: " << path << ", mode: " << std::oct << mode
               << ")";

    MetadataCache::UuidAccessor uuidAcc;
    MetadataCache::MetaAccessor metaAcc;
    m_metadataCache.getAttr(uuidAcc, metaAcc, path);

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            messages::fuse::ChangeMode{uuidAcc->second, mode});

    communication::wait(future);
    metaAcc->second.attr.get().mode(mode);

    return 0;
}

int FsLogic::chown(
    boost::filesystem::path path, const uid_t uid, const gid_t gid)
{
    DLOG(INFO) << "FUSE: chown(path: " << path << ", uid: " << uid
               << ", gid: " << gid << ")";

    throw std::errc::operation_not_supported;
}

int FsLogic::truncate(boost::filesystem::path path, const off_t newSize)
{
    DLOG(INFO) << "FUSE: truncate(path: " << path << ", newSize: " << newSize
               << ")";

    MetadataCache::MetaAccessor acc;
    m_metadataCache.getAttr(acc, path);
    auto &attr = acc->second.attr.get();

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            messages::fuse::Truncate{attr.uuid(), newSize});

    communication::wait(future);
    attr.size(newSize);

    m_metadataCache.getLocation(acc, attr.uuid());
    acc->second.location.get().blocks() &=
        boost::icl::discrete_interval<off_t>::right_open(0, newSize);

    m_eventManager->emitTruncateEvent(newSize, attr.uuid());

    return 0;
}

int FsLogic::utime(boost::filesystem::path path, struct utimbuf *const ubuf)
{
    DLOG(INFO) << "FUSE: utime(path: " << path << ", ...)";

    MetadataCache::UuidAccessor uuidAcc;
    MetadataCache::MetaAccessor metaAcc;
    m_metadataCache.getAttr(uuidAcc, metaAcc, path);

    messages::fuse::UpdateTimes msg{uuidAcc->second};

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
            std::move(msg));

    communication::wait(future);

    auto &attr = metaAcc->second.attr.get();
    attr.atime(msg.atime().get());
    attr.mtime(msg.mtime().get());

    return 0;
}

int FsLogic::open(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: open(path: " << path << ", ...)";

    auto attr = m_metadataCache.getAttr(path);
    m_metadataCache.getLocation(attr.uuid());

    FileContextCache::Accessor acc;
    m_fileContextCache.create(acc);

    fileInfo->direct_io = 1;
    fileInfo->fh = acc->first;

    acc->second.uuid = attr.uuid();
    acc->second.helperCtx.flags = fileInfo->flags;

    return 0;
}

int FsLogic::read(boost::filesystem::path path, asio::mutable_buffer buf,
    const off_t offset, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: read(path: " << path
               << ", bufferSize: " << asio::buffer_size(buf)
               << ", offset: " << offset << ", ...)";

    auto context = m_fileContextCache.get(fileInfo->fh);
    auto attr = m_metadataCache.getAttr(context.uuid);
    auto location = m_metadataCache.getLocation(context.uuid);

    const auto possibleRange =
        boost::icl::discrete_interval<off_t>::right_open(0, attr.size());

    const auto wantedRange = boost::icl::discrete_interval<off_t>::right_open(
                                 offset, offset + asio::buffer_size(buf)) &
        possibleRange;

    if (boost::icl::size(wantedRange) == 0)
        return 0;

    // Even if several "touching" blocks with different helpers are available to
    // read right now, for simplicity we'll only read a single block per a read
    // operation.
    auto availableBlockIt =
        location.blocks().find(boost::icl::discrete_interval<off_t>(offset));

    if (availableBlockIt == location.blocks().end())
        throw std::errc::bad_address; ///< @todo Waiting for blocks.

    const messages::fuse::FileBlock &fileBlock = availableBlockIt->second;
    auto availableRange = availableBlockIt->first & wantedRange;
    buf = asio::buffer(buf, boost::icl::size(availableRange));

    auto helper = getHelper(fileBlock.storageId());
    buf = HelperWrapper(*helper, context.helperCtx)
              .read({fileBlock.fileId()}, buf, offset);

    const auto bytesRead = asio::buffer_size(buf);
    m_eventManager->emitReadEvent(offset, bytesRead, context.uuid);

    return bytesRead;
}

int FsLogic::write(boost::filesystem::path path, asio::const_buffer buf,
    const off_t offset, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: write(path: " << path
               << ", bufferSize: " << asio::buffer_size(buf)
               << ", offset: " << offset << ", ...)";

    const auto wantedRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + asio::buffer_size(buf));

    if (boost::icl::size(wantedRange) == 0)
        return 0;

    auto context = m_fileContextCache.get(fileInfo->fh);
    auto attr = m_metadataCache.getAttr(context.uuid);
    auto location = m_metadataCache.getLocation(context.uuid);

    // Even if several "touching" blocks with different helpers are
    // available to write right now, for simplicity we'll only write to a
    // single block per a write operation.

    boost::icl::discrete_interval<off_t> offsetInterval{offset};
    auto availableBlockIt = location.blocks().lower_bound(offsetInterval);

    messages::fuse::FileBlock fileBlock{
        location.storageId(), location.fileId()};

    if (availableBlockIt == location.blocks().end()) {
        // pass, write everything to default locations
    }
    else if (boost::icl::contains(availableBlockIt->first, offsetInterval)) {
        fileBlock = availableBlockIt->second;
        buf = asio::buffer(
            buf, boost::icl::size(availableBlockIt->first & wantedRange));
    }
    else {
        auto blankRange = boost::icl::discrete_interval<off_t>::right_open(
            offset, boost::icl::first(availableBlockIt->first));

        buf = asio::buffer(buf, boost::icl::size(blankRange & wantedRange));
    }

    auto helper = getHelper(location.storageId());
    auto bytesWritten = HelperWrapper(*helper, context.helperCtx)
                            .write(location.fileId(), buf, offset);

    m_eventManager->emitWriteEvent(offset, bytesWritten, context.uuid,
        location.storageId(), location.fileId());

    MetadataCache::MetaAccessor acc;
    m_metadataCache.getAttr(acc, context.uuid);
    acc->second.attr.get().size(std::max(acc->second.attr.get().size(),
        static_cast<off_t>(offset + bytesWritten)));

    auto writtenRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + bytesWritten);

    // Call `getLocation` instead of using existing acc for a corner case
    // where location has been removed since initial `getLocation` call.
    m_metadataCache.getLocation(acc, context.uuid);
    acc->second.location.get().blocks() +=
        std::make_pair(writtenRange, std::move(fileBlock));

    return bytesWritten;
}

int FsLogic::statfs(
    boost::filesystem::path path, struct statvfs *const statInfo)
{
    DLOG(INFO) << "FUSE: statfs(path: " << path << ", ...)";
    throw std::errc::operation_not_supported;
}

int FsLogic::flush(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: flush(path: " << path << ", ...)";
    return 0;
}

int FsLogic::release(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: release(path: " << path << ", ...)";
    return 0;
}

int FsLogic::fsync(boost::filesystem::path path, const int datasync,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: fsync(path: " << path << ", datasync: " << datasync
               << ", ...)";

    throw std::errc::operation_not_supported;
}

int FsLogic::opendir(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: opendir(path: " << path << ", ...)";
    return 0;
}

int FsLogic::readdir(boost::filesystem::path path, void *const buf,
    const fuse_fill_dir_t filler, const off_t offset,
    struct fuse_file_info *const /*fileInfo*/)
{
    DLOG(INFO) << "FUSE: readdir(path: " << path << ", ..., offset: " << offset
               << ", ...)";

    auto attr = m_metadataCache.getAttr(path);
    if (attr.type() != messages::fuse::FileAttr::FileType::directory)
        throw std::errc::not_a_directory;

    messages::fuse::GetFileChildren msg{attr.uuid(), offset, 1000};
    auto future =
        m_context->communicator()->communicate<messages::fuse::FileChildren>(
            std::move(msg));

    auto fileChildren = communication::wait(future);
    auto currentOffset = offset;

    for (const auto &uuidAndName : fileChildren.uuidsAndNames()) {
        auto name = std::get<1>(uuidAndName);
        auto childPath = path / name;
        m_metadataCache.map(std::move(childPath), std::get<0>(uuidAndName));

        if (filler(buf, name.c_str(), nullptr, ++currentOffset))
            break;
    }

    return 0;
}

int FsLogic::releasedir(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: releasedir(path: " << path << ", ...)";
    return 0;
}

int FsLogic::fsyncdir(boost::filesystem::path path, const int datasync,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: fsyncdir(path: " << path << ", datasync: " << datasync
               << ", ...)";

    return 0;
}

HelpersCache::HelperPtr FsLogic::getHelper(
    const std::string &storageId, const bool forceClusterProxy)
{
    return m_helpersCache.get(storageId, forceClusterProxy);
}

void FsLogic::removeFile(boost::filesystem::path path)
{
    MetadataCache::UuidAccessor uuidAcc;
    MetadataCache::MetaAccessor metaAcc;
    m_metadataCache.getAttr(uuidAcc, metaAcc, path);

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            messages::fuse::DeleteFile{uuidAcc->second});

    communication::wait(future);

    m_metadataCache.remove(uuidAcc, metaAcc);
}

} // namespace client
} // namespace one
