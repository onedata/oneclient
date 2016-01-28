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
#include "logging.h"
#include "options.h"

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

#include <sys/stat.h>

#include <algorithm>
#include <memory>

using namespace std::literals;

namespace one {
namespace client {

FsLogic::FsLogic(
    std::shared_ptr<Context> context, events::SubscriptionContainer container)
    : m_uid{geteuid()}
    , m_gid{getegid()}
    , m_context{std::move(context)}
    , m_eventManager{m_context}
    , m_helpersCache{*m_context->communicator()}
    , m_metadataCache{*m_context->communicator()}
    , m_fsSubscriptions{*m_context->scheduler(), m_eventManager}
{
    m_eventManager.setFileAttrHandler(fileAttrHandler());
    m_eventManager.setFileLocationHandler(fileLocationHandler());
    m_eventManager.subscribe(std::move(container));
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

    m_fsSubscriptions.addTemporaryFileAttrSubscription(attr.uuid());

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

    const mode_t normalizedMode = mode & ALLPERMS;

    MetadataCache::UuidAccessor uuidAcc;
    MetadataCache::MetaAccessor metaAcc;
    m_metadataCache.getAttr(uuidAcc, metaAcc, path);

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            messages::fuse::ChangeMode{uuidAcc->second, normalizedMode});

    communication::wait(future);
    metaAcc->second.attr.get().mode(normalizedMode);

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

    m_eventManager.emitTruncateEvent(newSize, attr.uuid());

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
    auto location = m_metadataCache.getLocation(attr.uuid());
    auto helper = getHelper(location.spaceId(), location.storageId());

    FileContextCache::Accessor acc;
    m_fileContextCache.create(acc);

    fileInfo->direct_io = 1;
    fileInfo->fh = acc->first;

    acc->second.uuid = attr.uuid();
    acc->second.flags = fileInfo->flags;
    acc->second.helperCtxMap =
        std::make_shared<FileContextCache::HelperCtxMap>();

    m_fsSubscriptions.addFileLocationSubscription(attr.uuid());

    return 0;
}

/**
 * Opens file with new context for given {storageId, fileId}
 * @param ctxMapKey {storageId, fileId}
 * @param fileCtx
 * @param helper
 */
static void openFile(const FileContextCache::HelperCtxMapKey &ctxMapKey,
    FileContextCache::FileContext &fileCtx,
    const HelpersCache::HelperPtr &helper)
{
    auto helperCtx = helper->createCTX();
    helperCtx->setFlags(fileCtx.flags);

    helper->sh_open(helperCtx, ctxMapKey.second);

    FileContextCache::HelperCtxMapAccessor acc;
    fileCtx.helperCtxMap->insert(acc, ctxMapKey);
    acc->second = helperCtx;
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

    auto helper = getHelper(location.spaceId(), fileBlock.storageId());

    FileContextCache::HelperCtxMapKey ctxMapKey = {
        fileBlock.storageId(), fileBlock.fileId()};
    if (context.helperCtxMap->count(ctxMapKey) == 0)
        openFile(ctxMapKey, context, helper);
    FileContextCache::HelperCtxMapAccessor ctxAcc;
    context.helperCtxMap->find(ctxAcc, ctxMapKey);

    buf = helper->sh_read(ctxAcc->second, fileBlock.fileId(), buf, offset);

    const auto bytesRead = asio::buffer_size(buf);
    m_eventManager.emitReadEvent(offset, bytesRead, context.uuid);

    return bytesRead;
}

int FsLogic::write(boost::filesystem::path path, asio::const_buffer buf,
    const off_t offset, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: write(path: " << path
               << ", bufferSize: " << asio::buffer_size(buf)
               << ", offset: " << offset << ", ...)";

    if (asio::buffer_size(buf) == 0)
        return 0;

    auto context = m_fileContextCache.get(fileInfo->fh);
    auto attr = m_metadataCache.getAttr(context.uuid);
    auto location = m_metadataCache.getLocation(context.uuid);

    messages::fuse::FileBlock fileBlock;
    std::tie(fileBlock, buf) = findWriteLocation(location, offset, buf);

    auto helper = getHelper(location.spaceId(), location.storageId());

    FileContextCache::HelperCtxMapKey ctxMapKey = {
        location.storageId(), fileBlock.fileId()};
    if (context.helperCtxMap->count(ctxMapKey) == 0)
        openFile(ctxMapKey, context, helper);
    FileContextCache::HelperCtxMapAccessor ctxAcc;
    context.helperCtxMap->find(ctxAcc, ctxMapKey);

    auto bytesWritten =
        helper->sh_write(ctxAcc->second, fileBlock.fileId(), buf, offset);

    m_eventManager.emitWriteEvent(offset, bytesWritten, context.uuid,
        location.storageId(), fileBlock.fileId());

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

std::tuple<messages::fuse::FileBlock, asio::const_buffer>
FsLogic::findWriteLocation(const messages::fuse::FileLocation &fileLocation,
    const off_t offset, const asio::const_buffer &buf)
{
    const auto wantedRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + asio::buffer_size(buf));

    // Even if several "touching" blocks with different helpers are
    // available to write right now, for simplicity we'll only write to a
    // single block per a write operation.

    boost::icl::discrete_interval<off_t> offsetInterval{offset};
    auto availableBlockIt = fileLocation.blocks().lower_bound(offsetInterval);

    messages::fuse::FileBlock defaultBlock{
        fileLocation.storageId(), fileLocation.fileId()};

    if (availableBlockIt == fileLocation.blocks().end())
        return std::make_tuple(defaultBlock, buf);

    if (boost::icl::contains(availableBlockIt->first, offsetInterval))
        return std::make_tuple(availableBlockIt->second,
            asio::buffer(buf, boost::icl::size(
                                  availableBlockIt->first & wantedRange)));

    auto blankRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, boost::icl::first(availableBlockIt->first));

    return std::make_tuple(defaultBlock,
        asio::buffer(buf, boost::icl::size(blankRange & wantedRange)));
}

events::FileAttrEventStream::Handler FsLogic::fileAttrHandler()
{
    using namespace events;
    return [this](std::vector<FileAttrEventStream::EventPtr> events) {
        for (const auto &event : events) {
            const auto &newAttr = event->wrapped();
            MetadataCache::MetaAccessor acc;
            if (!m_metadataCache.get(acc, newAttr.uuid()) ||
                !acc->second.attr) {
                LOG(INFO) << "No attributes to update for uuid: '"
                          << newAttr.uuid() << "'";
                continue;
            }

            LOG(INFO) << "Updating attributes for uuid: '" << newAttr.uuid()
                      << "'";
            auto &attr = acc->second.attr.get();

            if (newAttr.size() < attr.size() && acc->second.location) {
                LOG(INFO) << "Truncating blocks attributes for uuid: '"
                          << newAttr.uuid() << "'";

                acc->second.location.get().blocks() &=
                    boost::icl::discrete_interval<off_t>::right_open(
                        0, newAttr.size());
            }

            attr.atime(std::max(attr.atime(), newAttr.atime()));
            attr.ctime(std::max(attr.ctime(), newAttr.ctime()));
            attr.mtime(std::max(attr.mtime(), newAttr.mtime()));
            attr.gid(newAttr.gid());
            attr.mode(newAttr.mode());
            attr.size(newAttr.size());
            attr.uid(newAttr.uid());
        }
    };
}

events::FileLocationEventStream::Handler FsLogic::fileLocationHandler()
{
    using namespace events;
    return [this](std::vector<FileLocationEventStream::EventPtr> events) {
        for (const auto &event : events) {
            const auto &newLocation = event->wrapped();
            MetadataCache::MetaAccessor acc;
            if (!m_metadataCache.get(acc, newLocation.uuid()) ||
                !acc->second.attr) {
                LOG(INFO) << "No location to update for uuid: '"
                          << newLocation.uuid() << "'";
                continue;
            }

            LOG(INFO) << "Updating location for uuid: '" << newLocation.uuid()
                      << "'";
            auto &location = acc->second.location.get();

            location.storageId(newLocation.storageId());
            location.fileId(newLocation.fileId());

            // `newLocation.blocks()` is on LHS of the expression, because
            // FileBlock keeps first value of {storageId, fileId} and ignores
            // any new values
            location.blocks() = newLocation.blocks() | location.blocks();
        }
    };
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
    auto attr = m_metadataCache.getAttr(path);
    m_fsSubscriptions.removeFileLocationSubscription(attr.uuid());

    auto context = m_fileContextCache.get(fileInfo->fh);
    std::exception_ptr systemErrorPtr;
    for (auto &it : *context.helperCtxMap) {
        auto &storageId = it.first.first;
        auto &fileId = it.first.second;
        auto helper = getHelper(storageId, fileId);
        try {
            helper->sh_release(it.second, fileId);
        }
        catch (std::system_error &e) {
            DLOG(WARNING) << "FUSE: release(storageId: " << storageId
                          << ", fileId: " << fileId << ") failed" << e.what();
            if (!systemErrorPtr) {
                systemErrorPtr = std::current_exception();
            }
        }
    }
    context.helperCtxMap->clear();
    if (systemErrorPtr)
        std::rethrow_exception(systemErrorPtr);
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
    const std::string &spaceId, const std::string &storageId)
{
    auto forceClusterProxy = !m_context->options()->get_directio();
    return m_helpersCache.get(spaceId, storageId, forceClusterProxy);
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
