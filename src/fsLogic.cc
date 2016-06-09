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
#include "directIOHelper.h"
#include "helpers/IStorageHelper.h"
#include "logging.h"
#include "options.h"

#include "messages/configuration.h"
#include "messages/fuse/changeMode.h"
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
#include "messages/fuse/release.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/synchronizeBlockAndComputeChecksum.h"
#include "messages/fuse/truncate.h"
#include "messages/fuse/updateTimes.h"

#include <boost/algorithm/string.hpp>
#include <openssl/md4.h>

#include <sys/stat.h>

#include <algorithm>
#include <memory>
#include <random>

using namespace std::literals;

namespace one {
namespace client {

namespace {
unsigned long getfsid()
{
    std::random_device device;
    std::default_random_engine engine{device()};
    std::uniform_int_distribution<unsigned long> distribution{1};
    return distribution(engine);
}
}

FsLogic::FsLogic(std::shared_ptr<Context> context,
    std::shared_ptr<messages::Configuration> configuration)
    : m_uid{geteuid()}
    , m_gid{getegid()}
    , m_fsid{getfsid()}
    , m_context{std::move(context)}
    , m_eventManager{m_context}
    , m_helpersCache{*m_context->communicator(), *m_context->scheduler()}
    , m_metadataCache{*m_context->communicator()}
    , m_fsSubscriptions{m_eventManager}
    , m_forceProxyIOCache{m_fsSubscriptions}
{
    m_eventManager.setFileAttrHandler(fileAttrHandler());
    m_eventManager.setFileLocationHandler(fileLocationHandler());
    m_eventManager.setPermissionChangedHandler(permissionChangedHandler());
    m_eventManager.setFileRemovalHandler(fileRemovalHandler());
    m_eventManager.setQuotaExeededHandler(quotaExeededHandler());
    m_eventManager.setFileRenamedHandler(fileRenamedHandler());
    m_eventManager.subscribe(configuration->subscriptionContainer());

    // Quota initial configuration
    m_fsSubscriptions.addQuotaSubscription();
    disableSpaces(configuration->disabledSpacesContainer());

    scheduleCacheExpirationTick();
}

FsLogic::~FsLogic()
{
    std::lock_guard<std::mutex> guard{m_cancelCacheExpirationTickMutex};
    m_cancelCacheExpirationTick();
}

void FsLogic::scheduleCacheExpirationTick()
{
    std::lock_guard<std::mutex> guard{m_cancelCacheExpirationTickMutex};
    m_cancelCacheExpirationTick = m_context->scheduler()->schedule(1s, [this] {
        m_locExpirationHelper.tick([this](const std::string &uuid) {
            m_metadataCache.remove(uuid);
            m_fsSubscriptions.removeFileLocationSubscription(uuid);
        });

        m_attrExpirationHelper.tick([this](const std::string &uuid) {
            m_metadataCache.remove(uuid);
            m_fsSubscriptions.removeFileAttrSubscription(uuid);
            m_fsSubscriptions.removeFileRemovalSubscription(uuid);
            m_fsSubscriptions.removeFileRenamedSubscription(uuid);
        });

        scheduleCacheExpirationTick();
    });
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
    statbuf->st_size = attr.size().get();
    statbuf->st_nlink = 1;
    statbuf->st_blocks = 0;

    switch (attr.type()) {
        case messages::fuse::FileAttr::FileType::directory:
            statbuf->st_mode |= S_IFDIR;
            // Remove sticky bit for nfs compatibility
            statbuf->st_mode &= ~S_ISVTX;
            break;
        case messages::fuse::FileAttr::FileType::link:
            statbuf->st_mode |= S_IFLNK;
            break;
        case messages::fuse::FileAttr::FileType::regular:
            statbuf->st_mode |= S_IFREG;
            break;
    }

    m_attrExpirationHelper.markInteresting(attr.uuid(), [&] {
        m_metadataCache.getAttr(attr.uuid());
        m_fsSubscriptions.addFileAttrSubscription(attr.uuid());
        m_fsSubscriptions.addFileRemovalSubscription(attr.uuid());
        m_fsSubscriptions.addFileRenamedSubscription(attr.uuid());
    });

    return 0;
}

int FsLogic::readlink(boost::filesystem::path path, asio::mutable_buffer buf)
{
    DLOG(INFO) << "FUSE: readlink(path: " << path
               << ", bufferSize: " << asio::buffer_size(buf) << ")";

    throw std::errc::function_not_supported;
}

int FsLogic::mknod(
    boost::filesystem::path path, const mode_t mode, const dev_t dev)
{
    DLOG(INFO) << "FUSE: mknod(path: " << path << ", mode: " << std::oct << mode
               << ", dev: " << dev << ")";

    createFile(std::move(path), mode, {one::helpers::Flag::RDWR});
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

    throw std::errc::function_not_supported;
}

int FsLogic::rename(
    boost::filesystem::path oldPath, boost::filesystem::path newPath)
{
    DLOG(INFO) << "FUSE: rename(oldpath: " << oldPath
               << ", newpath: " << newPath << "')";

    auto uuidChanges = m_metadataCache.rename(oldPath, newPath);
    for (auto &uuidChange : uuidChanges) {
        m_attrExpirationHelper.rename(uuidChange.first, uuidChange.second, [&] {
            m_metadataCache.getAttr(uuidChange.second);
            m_fsSubscriptions.addFileAttrSubscription(uuidChange.second);
            m_fsSubscriptions.addFileRemovalSubscription(uuidChange.second);
            m_fsSubscriptions.addFileRenamedSubscription(uuidChange.second);
        });
    }

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

    throw std::errc::function_not_supported;
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

    m_metadataCache.getLocation(acc, attr.uuid(), {one::helpers::Flag::WRONLY});
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

    const auto now = std::chrono::system_clock::now();
    msg.atime(ubuf->actime
            ? std::chrono::system_clock::from_time_t(ubuf->actime)
            : now);
    msg.mtime(ubuf->modtime
            ? std::chrono::system_clock::from_time_t(ubuf->modtime)
            : now);
    msg.ctime(now);

    auto future =
        m_context->communicator()->communicate<messages::fuse::FuseResponse>(
            std::move(msg));

    communication::wait(future);

    auto &attr = metaAcc->second.attr.get();
    attr.atime(msg.atime().get());
    attr.mtime(msg.mtime().get());
    attr.mtime(msg.ctime().get());

    return 0;
}

int FsLogic::open(
    boost::filesystem::path path, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: open(path: " << path << ", flags: " << fileInfo->flags
               << " ...)";

    auto attr = m_metadataCache.getAttr(path);
    auto location = m_metadataCache.getLocation(attr.uuid(),
        one::helpers::IStorageHelper::maskToFlags(fileInfo->flags));
    auto helper = getHelper(attr.uuid(), location.storageId());
    openFile(location.uuid(), fileInfo);

    return 0;
}

namespace {

std::unordered_map<std::string, std::string> makeParameters(
    FileContextCache::FileContext &fileCtx)
{
    std::unordered_map<std::string, std::string> parameters{
        {"file_uuid", fileCtx.uuid}};

    if (fileCtx.handleId->is_initialized())
        parameters.emplace("handle_id", fileCtx.handleId->get());

    return parameters;
};

int openFile(const FileContextCache::HelperCtxMapAccessor &ctxAcc,
    FileContextCache::FileContext &fileCtx,
    const HelpersCache::HelperPtr &helper, const std::string &fileId,
    std::unordered_map<std::string, std::string> parameters)
{
    auto helperCtx = helper->createCTX(std::move(parameters));
    ctxAcc->second = helperCtx;
    int fh = helper->sh_open(helperCtx, fileId, fileCtx.flags);
    return fh;
}

helpers::CTXPtr getHelperCtx(FileContextCache::FileContext &fileCtx,
    const HelpersCache::HelperPtr &helper, const std::string &storageId,
    const std::string &fileId)
{
    FileContextCache::HelperCtxMapKey ctxMapKey{storageId, fileId};
    FileContextCache::HelperCtxMapAccessor ctxAcc;
    if (fileCtx.helperCtxMap->insert(ctxAcc, std::move(ctxMapKey)))
        openFile(ctxAcc, fileCtx, helper, fileId, makeParameters(fileCtx));

    return ctxAcc->second;
}

} // namespace

int FsLogic::read(boost::filesystem::path path, asio::mutable_buffer buf,
    const off_t offset, struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: read(path: " << path
               << ", bufferSize: " << asio::buffer_size(buf)
               << ", offset: " << offset << ", ...)";

    auto context = m_fileContextCache.get(fileInfo->fh);
    auto attr = m_metadataCache.getAttr(context.uuid);
    auto location = m_metadataCache.getLocation(context.uuid,
        one::helpers::IStorageHelper::maskToFlags(fileInfo->flags));

    const auto possibleRange =
        boost::icl::discrete_interval<off_t>::right_open(0, attr.size().get());

    const auto requestedRange =
        boost::icl::discrete_interval<off_t>::right_open(
            offset, offset + asio::buffer_size(buf));

    const auto wantedRange = requestedRange & possibleRange;

    if (boost::icl::size(wantedRange) == 0)
        return 0;

    // Even if several "touching" blocks with different helpers are available to
    // read right now, for simplicity we'll only read a single block per a read
    // operation.
    auto availableBlockIt =
        location.blocks().find(boost::icl::discrete_interval<off_t>(offset));

    boost::optional<messages::fuse::Checksum> serverChecksum;
    auto flagSet = one::helpers::IStorageHelper::maskToFlags(fileInfo->flags);
    bool dataNeedsSynchronization = availableBlockIt == location.blocks().end();
    if (dataNeedsSynchronization) {
        serverChecksum =
            waitForBlockSynchronization(context.uuid, wantedRange, flagSet);
        location = m_metadataCache.getLocation(context.uuid, flagSet);
        availableBlockIt = location.blocks().find(
            boost::icl::discrete_interval<off_t>(offset));
    }

    const messages::fuse::FileBlock &fileBlock = availableBlockIt->second;
    auto availableRange = availableBlockIt->first & wantedRange;
    buf = asio::buffer(buf, boost::icl::size(availableRange));

    auto helper = getHelper(context.uuid, fileBlock.storageId());
    auto helperCtx = getHelperCtx(
        context, helper, fileBlock.storageId(), fileBlock.fileId());

    try {
        if (dataNeedsSynchronization) {
            helper->sh_flush(helperCtx, fileBlock.fileId());
        }

        auto readBuffer =
            helper->sh_read(helperCtx, fileBlock.fileId(), buf, offset);

        if (helper->needsDataConsistencyCheck() && dataNeedsSynchronization &&
            dataCorrupted(context.uuid, readBuffer, serverChecksum.get(),
                availableRange, wantedRange)) {
            helper->sh_release(helperCtx,
                fileBlock.fileId()); // close the file to get data up to date,
                                     // it will be opened again by read function
            return read(path, buf, offset, fileInfo);
        }

        const auto bytesRead = asio::buffer_size(readBuffer);
        m_eventManager.emitReadEvent(offset, bytesRead, context.uuid);

        return bytesRead;
    }
    catch (const std::system_error &e) {
        if (e.code().value() != EPERM && e.code().value() != EACCES)
            throw;
        if (m_forceProxyIOCache.contains(context.uuid))
            throw;

        m_forceProxyIOCache.insert(context.uuid);
        return read(path, buf, offset, fileInfo);
    }
}

messages::fuse::Checksum FsLogic::waitForBlockSynchronization(
    const std::string &uuid, const boost::icl::discrete_interval<off_t> &range,
    const one::helpers::FlagsSet flags)
{
    auto checksum = syncAndFetchChecksum(uuid, range);

    if (!m_metadataCache.waitForNewLocation(uuid, range,
            std::chrono::seconds{m_context->options()->get_file_sync_timeout()},
            flags))
        throw std::errc::resource_unavailable_try_again;

    return checksum;
}

messages::fuse::Checksum FsLogic::syncAndFetchChecksum(
    const std::string &uuid, const boost::icl::discrete_interval<off_t> &range)
{
    messages::fuse::SynchronizeBlockAndComputeChecksum request{uuid, range};
    auto future =
        m_context->communicator()->communicate<messages::fuse::Checksum>(
            std::move(request));
    return communication::wait(future);
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
    auto location = m_metadataCache.getLocation(context.uuid,
        one::helpers::IStorageHelper::maskToFlags(fileInfo->flags));

    // Check if this space is marked as disabled due to exeeded quota
    {
        std::shared_lock<std::shared_timed_mutex> lock{m_disabledSpacesMutex};
        if (m_disabledSpaces.count(location.spaceId()))
            return -ENOSPC;
    }

    messages::fuse::FileBlock fileBlock;
    std::tie(fileBlock, buf) = findWriteLocation(location, offset, buf);

    auto helper = getHelper(context.uuid, location.storageId());
    auto helperCtx = getHelperCtx(
        context, helper, fileBlock.storageId(), fileBlock.fileId());

    size_t bytesWritten = 0;
    try {
        bytesWritten =
            helper->sh_write(helperCtx, fileBlock.fileId(), buf, offset);
    }
    catch (const std::system_error &e) {
        if (e.code().value() != EPERM && e.code().value() != EACCES)
            throw;
        if (m_forceProxyIOCache.contains(context.uuid))
            throw;

        m_forceProxyIOCache.insert(context.uuid);
        return write(path, buf, offset, fileInfo);
    }

    m_eventManager.emitWriteEvent(offset, bytesWritten, context.uuid,
        location.storageId(), fileBlock.fileId());

    MetadataCache::MetaAccessor acc;
    m_metadataCache.getAttr(acc, context.uuid);
    acc->second.attr.get().size(std::max(acc->second.attr.get().size().get(),
        static_cast<off_t>(offset + bytesWritten)));

    auto writtenRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + bytesWritten);

    // Call `getLocation` instead of using existing acc for a corner case
    // where location has been removed since initial `getLocation` call.
    m_metadataCache.getLocation(acc, context.uuid,
        one::helpers::IStorageHelper::maskToFlags(fileInfo->flags));
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
                      << "', size: " << (newAttr.size().is_initialized()
                                                ? newAttr.size().get()
                                                : -1);
            auto &attr = acc->second.attr.get();

            if (newAttr.size().is_initialized() &&
                newAttr.size().get() < attr.size() && acc->second.location) {
                LOG(INFO) << "Truncating blocks attributes for uuid: '"
                          << newAttr.uuid() << "'";

                acc->second.location.get().blocks() &=
                    boost::icl::discrete_interval<off_t>::right_open(
                        0, newAttr.size().get());
            }

            attr.atime(std::max(attr.atime(), newAttr.atime()));
            attr.ctime(std::max(attr.ctime(), newAttr.ctime()));
            attr.mtime(std::max(attr.mtime(), newAttr.mtime()));
            attr.gid(newAttr.gid());
            attr.mode(newAttr.mode());
            if (newAttr.size().is_initialized())
                attr.size(newAttr.size().get());
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
                !acc->second.location) {
                LOG(INFO) << "No location to update for uuid: '"
                          << newLocation.uuid() << "'";
                continue;
            }

            LOG(INFO) << "Updating location for uuid: '" << newLocation.uuid()
                      << "'";
            auto &location = acc->second.location.get();

            location.storageId(newLocation.storageId());
            location.fileId(newLocation.fileId());

            location.blocks() = newLocation.blocks();

            m_metadataCache.notifyNewLocationArrived(newLocation.uuid());
        }
    };
}

events::PermissionChangedEventStream::Handler
FsLogic::permissionChangedHandler()
{
    using namespace events;
    return [this](std::vector<PermissionChangedEventStream::EventPtr> events) {
        for (const auto &event : events) {
            LOG(INFO) << "Invalidating forceProxyIOCache for uuid: '"
                      << event->fileUuid() << "'";
            m_forceProxyIOCache.erase(event->fileUuid());
        }
    };
}

int FsLogic::statfs(
    boost::filesystem::path path, struct statvfs *const statInfo)
{
    DLOG(INFO) << "FUSE: statfs(path: " << path << ", ...)";

    *statInfo = {};
    statInfo->f_fsid = m_fsid;
    return 0;
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

    m_locExpirationHelper.unpin(attr.uuid());
    m_attrExpirationHelper.unpin(attr.uuid());

    auto context = m_fileContextCache.get(fileInfo->fh);
    std::exception_ptr lastReleaseException;
    for (auto &it : *context.helperCtxMap) {
        auto &storageId = it.first.first;
        auto &fileId = it.first.second;
        auto helper = getHelper(attr.uuid(), storageId);
        try {
            helper->sh_release(it.second, fileId);
        }
        catch (std::system_error &e) {
            LOG(ERROR) << "release(storageId: " << storageId
                       << ", fileId: " << fileId << ") failed" << e.what();
            lastReleaseException = std::current_exception();
        }
    }

    if (context.handleId->is_initialized()) {
        auto future = m_context->communicator()
                          ->communicate<messages::fuse::FuseResponse>(
                              messages::fuse::Release{
                                  attr.uuid(), context.handleId->get()});

        communication::wait(future);

        *context.handleId = boost::none;
    }
    context.helperCtxMap->clear();

    if (lastReleaseException)
        std::rethrow_exception(lastReleaseException);
    return 0;
}

int FsLogic::fsync(boost::filesystem::path path, const int datasync,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: fsync(path: " << path << ", datasync: " << datasync
               << ", ...)";

    throw std::errc::function_not_supported;
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

    auto currentOffset = offset;

    std::size_t chunkSize = 1000;
    if (offset == 0) {
        chunkSize -= 2;
        (filler(buf, ".", nullptr, ++currentOffset));
        (filler(buf, "..", nullptr, ++currentOffset));
    }

    messages::fuse::GetFileChildren msg{attr.uuid(), offset, chunkSize};
    auto future =
        m_context->communicator()->communicate<messages::fuse::FileChildren>(
            std::move(msg));

    auto fileChildren = communication::wait(future);

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

int FsLogic::create(boost::filesystem::path path, const mode_t mode,
    struct fuse_file_info *const fileInfo)
{
    DLOG(INFO) << "FUSE: create(path: " << path << ", mode: " << std::oct
               << mode << ")";

    // Potential race condition, file might be modified between create and open
    auto fileUuid = createFile(std::move(path), mode,
        one::helpers::IStorageHelper::maskToFlags(fileInfo->flags));
    openFile(fileUuid, fileInfo);

    return 0;
}

HelpersCache::HelperPtr FsLogic::getHelper(
    const std::string &fileUuid, const std::string &storageId)
{
    auto forceProxyIO = m_context->options()->get_proxyio() ||
        m_forceProxyIOCache.contains(fileUuid);
    return m_helpersCache.get(fileUuid, storageId, forceProxyIO);
}

void FsLogic::removeFile(boost::filesystem::path path)
{
    MetadataCache::UuidAccessor uuidAcc;
    MetadataCache::MetaAccessor metaAcc;
    m_metadataCache.getAttr(uuidAcc, metaAcc, path);

    if (metaAcc->second.state != MetadataCache::FileState::removedUpstream) {
        auto future = m_context->communicator()
                          ->communicate<messages::fuse::FuseResponse>(
                              messages::fuse::DeleteFile{uuidAcc->second});

        communication::wait(future);
    }
    m_metadataCache.removePathMapping(uuidAcc, metaAcc);
}

const std::string FsLogic::createFile(boost::filesystem::path path,
    const mode_t mode, const one::helpers::FlagsSet flags)
{
    auto parentAttr = m_metadataCache.getAttr(path.parent_path());
    if (parentAttr.type() != messages::fuse::FileAttr::FileType::directory)
        throw std::errc::not_a_directory;

    messages::fuse::GetNewFileLocation msg{
        path.filename().string(), parentAttr.uuid(), mode, flags};

    auto future =
        m_context->communicator()->communicate<messages::fuse::FileLocation>(
            std::move(msg));

    auto location = communication::wait(future);
    m_metadataCache.map(path, location);

    return location.uuid();
}

void FsLogic::openFile(
    const std::string &fileUuid, struct fuse_file_info *const fileInfo)
{
    FileContextCache::Accessor acc;
    m_fileContextCache.create(acc);

    MetadataCache::MetaAccessor metaAcc;
    m_metadataCache.getLocation(metaAcc, fileUuid,
        one::helpers::IStorageHelper::maskToFlags(fileInfo->flags));
    auto &location = metaAcc->second.location.get();

    fileInfo->direct_io = 1;
    fileInfo->fh = acc->first;

    acc->second.uuid = fileUuid;
    acc->second.flags = fileInfo->flags & (~O_CREAT) & (~O_APPEND);
    acc->second.handleId =
        std::make_shared<boost::optional<std::string>>(location.handleId());
    // TODO: VFS-1959
    location.unsetHandleId();
    acc->second.helperCtxMap =
        std::make_shared<FileContextCache::HelperCtxMap>();

    metaAcc.release();

    m_locExpirationHelper.pin(fileUuid, [&] {
        m_metadataCache.getLocation(fileUuid,
            one::helpers::IStorageHelper::maskToFlags(fileInfo->flags));
        m_fsSubscriptions.addFileLocationSubscription(fileUuid);
    });

    m_attrExpirationHelper.pin(fileUuid, [&] {
        m_metadataCache.getAttr(fileUuid);
        m_fsSubscriptions.addFileAttrSubscription(fileUuid);
        m_fsSubscriptions.addFileRemovalSubscription(fileUuid);
        m_fsSubscriptions.addFileRenamedSubscription(fileUuid);
    });
}

events::FileRemovalEventStream::Handler FsLogic::fileRemovalHandler()
{
    using namespace events;
    return [this](std::vector<FileRemovalEventStream::EventPtr> events) {
        for (const auto &event : events) {

            MetadataCache::MetaAccessor metaAcc;
            m_metadataCache.getAttr(metaAcc, event->fileUuid());
            metaAcc->second.state = MetadataCache::FileState::removedUpstream;

            if (metaAcc->second.path) {
                auto path = metaAcc->second.path.get();
                metaAcc.release();
                try {
                    auto dir = m_context->options()->get_mountpoint();
                    std::remove((dir / path).c_str());
                }
                catch (std::system_error &e) {
                    LOG(WARNING) << "Unable to remove file (path: " << path
                                 << "): " << e.what();
                }
            }

            m_locExpirationHelper.expire(event->fileUuid());
            m_attrExpirationHelper.expire(event->fileUuid());
            LOG(INFO) << "File remove event received: " << event->fileUuid();
        }
    };
}

events::QuotaExeededEventStream::Handler FsLogic::quotaExeededHandler()
{
    using namespace events;
    return [this](std::vector<QuotaExeededEventStream::EventPtr> events) {
        if (!events.empty())
            disableSpaces(events.back()->spaces());
    };
}

events::FileRenamedEventStream::Handler FsLogic::fileRenamedHandler()
{
    using namespace events;
    return [this](std::vector<FileRenamedEventStream::EventPtr> events) {
        for (const auto &event : events) {
            auto topEntry = event->topEntry();
            MetadataCache::MetaAccessor metaAcc;
            m_metadataCache.getAttr(metaAcc, topEntry.oldUuid());
            metaAcc->second.state = MetadataCache::FileState::renamedUpstream;
            auto fromPath = metaAcc->second.path.get();
            metaAcc.release();

            auto dir = m_context->options()->get_mountpoint();
            auto toPath = boost::filesystem::path(topEntry.newPath());

            try {
                std::rename((dir / fromPath).c_str(), (dir / toPath).c_str());
            }
            catch (std::system_error &e) {
                LOG(WARNING) << "Unable to rename file (from: " << fromPath
                             << " to: " << toPath << "): " << e.what();
            }

            m_metadataCache.remapFile(
                topEntry.oldUuid(), topEntry.newUuid(), topEntry.newPath());

            m_attrExpirationHelper.rename(
                topEntry.oldUuid(), topEntry.newUuid(), [&] {
                    m_metadataCache.getAttr(topEntry.newUuid());
                    m_fsSubscriptions.addFileAttrSubscription(
                        topEntry.newUuid());
                    m_fsSubscriptions.addFileRemovalSubscription(
                        topEntry.newUuid());
                    m_fsSubscriptions.addFileRenamedSubscription(
                        topEntry.newUuid());
                });

            for (auto &childEntry : event->childEntries()) {
                m_metadataCache.remapFile(childEntry.oldUuid(),
                    childEntry.newUuid(), childEntry.newPath());

                m_attrExpirationHelper.rename(
                    childEntry.oldUuid(), childEntry.newUuid(), [&] {
                        m_metadataCache.getAttr(childEntry.newUuid());
                        m_fsSubscriptions.addFileAttrSubscription(
                            childEntry.newUuid());
                        m_fsSubscriptions.addFileRemovalSubscription(
                            childEntry.newUuid());
                        m_fsSubscriptions.addFileRenamedSubscription(
                            childEntry.newUuid());
                    });
            }

            LOG(INFO) << "File renamed event received: " << topEntry.oldUuid();
        }
    };
}

bool FsLogic::dataCorrupted(const std::string &uuid, asio::const_buffer buf,
    const messages::fuse::Checksum &serverChecksum,
    const boost::icl::discrete_interval<off_t> &availableRange,
    const boost::icl::discrete_interval<off_t> &wantedRange)
{
    auto checksum = availableRange == wantedRange
        ? computeHash(buf)
        : syncAndFetchChecksum(uuid, availableRange).value();
    if (serverChecksum.value() != checksum)
        return true;

    return false;
}

std::string FsLogic::computeHash(asio::const_buffer buf)
{
    std::string hash(MD4_DIGEST_LENGTH, '\0');
    MD4_CTX ctx;
    MD4_Init(&ctx);
    MD4_Update(
        &ctx, asio::buffer_cast<const char *>(buf), asio::buffer_size(buf));
    MD4_Final(reinterpret_cast<unsigned char *>(&hash[0]), &ctx);
    return hash;
}

void FsLogic::disableSpaces(const std::vector<std::string> &spaces)
{
    std::lock_guard<std::shared_timed_mutex> lock{m_disabledSpacesMutex};
    m_disabledSpaces = {spaces.begin(), spaces.end()};
}

} // namespace client
} // namespace one
