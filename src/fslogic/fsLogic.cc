/**
 * @file fsLogic.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsLogic.h"

#include "communication/communicator.h"
#include "context.h"
#include "messages/configuration.h"
#include "messages/fuse/changeMode.h"
#include "messages/fuse/createDir.h"
#include "messages/fuse/createFile.h"
#include "messages/fuse/deleteFile.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileBlock.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileCreated.h"
#include "messages/fuse/fileLocation.h"
#include "messages/fuse/fileOpened.h"
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/fileRenamedEntry.h"
#include "messages/fuse/getFileChildren.h"
#include "messages/fuse/makeFile.h"
#include "messages/fuse/openFile.h"
#include "messages/fuse/release.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/syncResponse.h"
#include "messages/fuse/synchronizeBlockAndComputeChecksum.h"
#include "messages/fuse/truncate.h"
#include "messages/fuse/updateTimes.h"
#include "options.h"

#include <boost/icl/interval_set.hpp>
#include <folly/fibers/FiberManager.h>
#include <folly/fibers/ForEach.h>
#include <fuse/fuse_lowlevel.h>
#include <openssl/md4.h>

namespace one {
namespace client {
namespace fslogic {

/**
 * Filters given flags set to one of RDONLY, WRONLY or RDWR.
 * Returns RDONLY if flag value is zero.
 * @param Flags value
 */
inline helpers::Flag getOpenFlag(const helpers::FlagsSet &flagsSet)
{
    if (flagsSet.count(one::helpers::Flag::RDONLY))
        return one::helpers::Flag::RDONLY;
    if (flagsSet.count(one::helpers::Flag::WRONLY))
        return one::helpers::Flag::WRONLY;
    if (flagsSet.count(one::helpers::Flag::RDWR))
        return one::helpers::Flag::RDWR;

    return one::helpers::Flag::RDONLY;
}

FsLogic::FsLogic(std::shared_ptr<Context> context,
    std::shared_ptr<messages::Configuration> configuration,
    std::unique_ptr<cache::HelpersCache> helpersCache,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_context{std::move(context)}
    , m_metadataCache{*m_context->communicator()}
    , m_helpersCache{std::move(helpersCache)}
    , m_fsSubscriptions{
          m_eventManager, m_metadataCache, m_forceProxyIOCache, runInFiber}
{
    using namespace std::placeholders;

    m_eventManager.subscribe(*configuration);

    // Quota initial configuration
    m_fsSubscriptions.subscribeQuotaExceeded([=](auto events) {
        runInFiber([ this, events = std::move(events) ] {
            auto e = events::get<events::QuotaExceededEvent>(events.back());
            this->disableSpaces(e->spaces());
        });
    });
    disableSpaces(configuration->disabledSpaces());

    m_forceProxyIOCache.onAdd([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.subscribePermissionChanged(uuid);
    });

    m_forceProxyIOCache.onRemove([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.unsubscribePermissionChanged(uuid);
    });

    m_metadataCache.onAdd([this](const folly::fbstring &uuid) {
        DLOG(INFO) << "metadataCache.onAdd";
        DLOG(INFO) << "1 subscribeFileAttrChanged";
        m_fsSubscriptions.subscribeFileAttrChanged(uuid);
        DLOG(INFO) << "2 subscribeFileRemoved";
        m_fsSubscriptions.subscribeFileRemoved(uuid);
        DLOG(INFO) << "3 subscribeFileRenamed";
        m_fsSubscriptions.subscribeFileRenamed(uuid);
        DLOG(INFO) << "metadataCache.onAdd DONE";
    });

    m_metadataCache.onOpen([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.subscribeFileLocationChanged(uuid);
    });

    m_metadataCache.onPrune([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.unsubscribeFileAttrChanged(uuid);
        m_fsSubscriptions.unsubscribeFileLocationChanged(uuid);
        m_fsSubscriptions.unsubscribeFileRemoved(uuid);
        m_fsSubscriptions.unsubscribeFileRenamed(uuid);
    });

    m_metadataCache.onRename(
        [this](const folly::fbstring &oldUuid, const folly::fbstring &newUuid) {
            m_fsSubscriptions.unsubscribeFileAttrChanged(oldUuid);
            m_fsSubscriptions.unsubscribeFileRemoved(oldUuid);
            m_fsSubscriptions.unsubscribeFileRenamed(oldUuid);
            m_fsSubscriptions.subscribeFileAttrChanged(newUuid);
            m_fsSubscriptions.subscribeFileRemoved(newUuid);
            m_fsSubscriptions.subscribeFileRenamed(newUuid);

            if (m_fsSubscriptions.unsubscribeFileLocationChanged(oldUuid))
                m_fsSubscriptions.subscribeFileLocationChanged(newUuid);

            m_onRename(oldUuid, newUuid);
        });

    m_metadataCache.onMarkDeleted(
        [this](const folly::fbstring &uuid) { m_onMarkDeleted(uuid); });
}

FileAttrPtr FsLogic::lookup(
    const folly::fbstring &uuid, const folly::fbstring &name)
{
    return m_metadataCache.getAttr(uuid, name);
}

FileAttrPtr FsLogic::getattr(const folly::fbstring &uuid)
{
    return m_metadataCache.getAttr(uuid);
}

folly::fbvector<folly::fbstring> FsLogic::readdir(const folly::fbstring &uuid)
{
    const std::size_t chunkSize = 1000;

    folly::fbvector<folly::fbstring> acc{".", ".."};

    for (off_t offset = 0;; offset += chunkSize) {
        auto msg = communicate<messages::fuse::FileChildren>(
            messages::fuse::GetFileChildren{uuid, offset, chunkSize});

        acc.insert(acc.end(), msg.children().begin(), msg.children().end());
        if (msg.children().size() < chunkSize)
            break;
    }

    return acc;
}

std::uint64_t FsLogic::open(const folly::fbstring &uuid, const int flags)
{
    auto openFileToken = m_metadataCache.open(uuid);

    const auto filteredFlags = flags & (~O_CREAT) & (~O_APPEND);

    const auto flag = getOpenFlag(helpers::maskToFlags(filteredFlags));
    messages::fuse::OpenFile msg{uuid.toStdString(), flag};

    auto opened = communicate<messages::fuse::FileOpened>(std::move(msg));

    const auto fuseFileHandleId = m_nextFuseHandleId++;
    m_fuseFileHandles.emplace(fuseFileHandleId,
        std::make_shared<FuseFileHandle>(filteredFlags, opened.handleId(),
            openFileToken, *m_helpersCache, m_forceProxyIOCache));

    return fuseFileHandleId;
}

void FsLogic::release(
    const folly::fbstring &uuid, const std::uint64_t fileHandleId)
{
    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);

    folly::fbvector<folly::Future<folly::Unit>> releaseFutures;
    for (auto &helperHandle : fuseFileHandle->helperHandles())
        releaseFutures.emplace_back(helperHandle->release());

    auto releaseExceptionFuture =
        folly::collectAll(releaseFutures)
            .then([](const std::vector<folly::Try<folly::Unit>> &tries) {
                for (auto &t : tries)
                    t.value();
            });

    std::exception_ptr releaseException;
    try {
        communication::wait(releaseExceptionFuture);
    }
    catch (...) {
        releaseException = std::current_exception();
    }

    communicate(messages::fuse::Release{
        uuid.toStdString(), fuseFileHandle->providerHandleId()->toStdString()});

    m_fuseFileHandles.erase(fileHandleId);

    if (releaseException)
        std::rethrow_exception(releaseException);
}

void FsLogic::flush(
    const folly::fbstring &uuid, const std::uint64_t fileHandleId)
{
    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);
    for (auto &helperHandle : fuseFileHandle->helperHandles())
        communication::wait(helperHandle->flush());
}

void FsLogic::fsync(const folly::fbstring &uuid,
    const std::uint64_t fileHandleId, const bool dataOnly)
{
    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);
    for (auto &helperHandle : fuseFileHandle->helperHandles())
        communication::wait(helperHandle->fsync(dataOnly));
}

folly::IOBufQueue FsLogic::read(const folly::fbstring &uuid,
    const std::uint64_t fileHandleId, const off_t offset,
    const std::size_t size, folly::Optional<folly::fbstring> checksum)
{
    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);
    auto attr = m_metadataCache.getAttr(uuid);

    const auto possibleRange =
        boost::icl::discrete_interval<off_t>::right_open(0, *attr->size());

    const auto requestedRange =
        boost::icl::discrete_interval<off_t>::right_open(offset, offset + size);

    const auto wantedRange = requestedRange & possibleRange;

    if (boost::icl::size(wantedRange) == 0)
        return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};

    // Even if several "touching" blocks with different helpers are available to
    // read right now, for simplicity we'll only read a single block per a read
    // operation.

    auto locationData = m_metadataCache.getBlock(uuid, offset);
    if (!locationData.hasValue()) {
        auto csum = syncAndFetchChecksum(uuid, wantedRange);
        return read(uuid, fileHandleId, offset, size, std::move(csum));
    }

    boost::icl::discrete_interval<off_t> availableRange;
    messages::fuse::FileBlock fileBlock;
    std::tie(availableRange, fileBlock) = std::move(*locationData);
    availableRange = availableRange & wantedRange;

    const std::size_t availableSize = boost::icl::size(availableRange);

    try {
        auto helperHandle = fuseFileHandle->getHelperHandle(
            uuid, fileBlock.storageId(), fileBlock.fileId());

        if (checksum)
            communication::wait(helperHandle->flush());

        auto readBuffer =
            communication::wait(helperHandle->read(offset, availableSize));

        if (helperHandle->needsDataConsistencyCheck() && checksum &&
            dataCorrupted(
                uuid, readBuffer, *checksum, availableRange, wantedRange)) {

            // close the file to get data up to date, it will be opened
            // again by read function
            fuseFileHandle->releaseHelperHandle(
                uuid, fileBlock.storageId(), fileBlock.fileId());

            return read(uuid, fileHandleId, offset, size, checksum);
        }

        const auto bytesRead = readBuffer.chainLength();
        m_eventManager.emit(std::make_shared<const events::ReadEvent>(
            uuid.toStdString(), offset, bytesRead));

        return readBuffer;
    }
    catch (const std::system_error &e) {
        if (e.code().value() != EPERM && e.code().value() != EACCES)
            throw;
        if (m_forceProxyIOCache.contains(uuid))
            throw;

        m_forceProxyIOCache.add(uuid);

        return read(uuid, fileHandleId, offset, size, checksum);
    }
}

std::size_t FsLogic::write(const folly::fbstring &uuid,
    const std::uint64_t fuseFileHandleId, const off_t offset,
    folly::IOBufQueue buf)
{
    if (buf.empty())
        return 0;

    auto fuseFileHandle = m_fuseFileHandles.at(fuseFileHandleId);
    auto attr = m_metadataCache.getAttr(uuid);

    // Check if this space is marked as disabled due to exeeded quota
    if (isSpaceDisabled(m_metadataCache.getSpaceId(uuid)))
        return -ENOSPC;

    auto fileBlock = m_metadataCache.getDefaultBlock(uuid);

    size_t bytesWritten = 0;
    try {
        auto helperHandle = fuseFileHandle->getHelperHandle(
            uuid, fileBlock.storageId(), fileBlock.fileId());

        bytesWritten =
            communication::wait(helperHandle->write(offset, std::move(buf)));
    }
    catch (const std::system_error &e) {
        if (e.code().value() != EPERM && e.code().value() != EACCES)
            throw;
        if (m_forceProxyIOCache.contains(uuid))
            throw;

        m_forceProxyIOCache.add(uuid);
        return write(uuid, fuseFileHandleId, offset, std::move(buf));
    }

    m_eventManager.emit(
        std::make_shared<const events::WriteEvent>(uuid.toStdString(), offset,
            bytesWritten, fileBlock.storageId(), fileBlock.fileId()));

    auto writtenRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + bytesWritten);

    m_metadataCache.addBlock(uuid, writtenRange, std::move(fileBlock));

    return bytesWritten;
}

FileAttrPtr FsLogic::mkdir(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const mode_t mode)
{
    // TODO: CreateDir should probably also return attrs
    communicate(messages::fuse::CreateDir{
        parentUuid.toStdString(), name.toStdString(), mode});

    // TODO: Provider returns uuid of the created dir, no need for lookup
    return m_metadataCache.getAttr(parentUuid, name);
}

FileAttrPtr FsLogic::mknod(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const mode_t mode)
{
    messages::fuse::MakeFile msg{parentUuid, name, mode};
    auto attr = communicate<FileAttr>(std::move(msg));
    auto sharedAttr = std::make_shared<FileAttr>(std::move(attr));
    m_metadataCache.putAttr(sharedAttr);

    return sharedAttr;
}

std::pair<FileAttrPtr, std::uint64_t> FsLogic::create(
    const folly::fbstring &parentUuid, const folly::fbstring &name,
    const mode_t mode, const int flags)
{
    const auto flag = getOpenFlag(helpers::maskToFlags(flags));
    messages::fuse::CreateFile msg{parentUuid, name, mode, flag};

    auto created = communicate<messages::fuse::FileCreated>(std::move(msg));

    const auto &uuid = created.attr().uuid();
    auto sharedAttr = std::make_shared<FileAttr>(std::move(created.attr()));
    auto location = std::make_unique<FileLocation>(created.location());
    auto openFileToken =
        m_metadataCache.open(uuid, sharedAttr, std::move(location));

    const auto fuseFileHandleId = m_nextFuseHandleId++;
    m_fuseFileHandles.emplace(fuseFileHandleId,
        std::make_shared<FuseFileHandle>(flags, created.handleId(),
            openFileToken, *m_helpersCache, m_forceProxyIOCache));

    return {sharedAttr, fuseFileHandleId};
}

void FsLogic::unlink(
    const folly::fbstring &parentUuid, const folly::fbstring &name)
{
    // TODO: directly order provider to delete {parentUuid, name}
    auto attr = m_metadataCache.getAttr(parentUuid, name);
    communicate(messages::fuse::DeleteFile{attr->uuid().toStdString()});
    m_metadataCache.markDeleted(attr->uuid());
}

void FsLogic::rename(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const folly::fbstring &newParentUuid,
    const folly::fbstring &newName)
{
    // TODO: directly order provider to rename {parentUuid, name}
    auto attr = m_metadataCache.getAttr(parentUuid, name);
    auto oldUuid = attr->uuid();

    auto renamed = communicate<messages::fuse::FileRenamed>(
        messages::fuse::Rename{oldUuid.toStdString(),
            newParentUuid.toStdString(), newName.toStdString()});

    m_metadataCache.rename(oldUuid, newParentUuid, newName, renamed.newUuid());

    for (auto &child : renamed.childEntries())
        m_metadataCache.rename(child.oldUuid(), child.newParentUuid(),
            child.newName(), child.newUuid());
}

FileAttrPtr FsLogic::setattr(
    const folly::fbstring &uuid, const struct stat &attr, const int toSet)
{
    // TODO: this operation can be optimized with a single message to the
    // provider

    if (toSet & FUSE_SET_ATTR_UID || toSet & FUSE_SET_ATTR_GID)
        throw std::errc::operation_not_supported;

    if (toSet & FUSE_SET_ATTR_MODE) {
        // ALLPERMS is a macro of sys/stat.h
        const mode_t normalizedMode = attr.st_mode & ALLPERMS;

        communicate(
            messages::fuse::ChangeMode{uuid.toStdString(), normalizedMode});

        m_metadataCache.changeMode(uuid, normalizedMode);
    }

    if (toSet & FUSE_SET_ATTR_SIZE) {
        communicate(messages::fuse::Truncate{uuid.toStdString(), attr.st_size});
        m_metadataCache.truncate(uuid, attr.st_size);
        m_eventManager.emit(std::make_shared<const events::TruncateEvent>(
            uuid.toStdString(), attr.st_size));
    }

    messages::fuse::UpdateTimes updateTimes{uuid.toStdString()};

    const auto now = std::chrono::system_clock::now();
    updateTimes.ctime(now);
    if (toSet & FUSE_SET_ATTR_ATIME)
        updateTimes.atime(
            std::chrono::system_clock::from_time_t(attr.st_atime));
    if (toSet & FUSE_SET_ATTR_MTIME)
        updateTimes.mtime(
            std::chrono::system_clock::from_time_t(attr.st_mtime));
    if (toSet & FUSE_SET_ATTR_ATIME_NOW)
        updateTimes.atime(now);
    if (toSet & FUSE_SET_ATTR_MTIME_NOW)
        updateTimes.mtime(now);

    communicate(updateTimes);
    m_metadataCache.updateTimes(uuid, updateTimes);

    return m_metadataCache.getAttr(uuid);
}

template <typename SrvMsg, typename CliMsg>
SrvMsg FsLogic::communicate(CliMsg &&msg)
{
    return communication::wait(m_context->communicator()->communicate<SrvMsg>(
        std::forward<CliMsg>(msg)));
}

folly::fbstring FsLogic::syncAndFetchChecksum(const folly::fbstring &uuid,
    const boost::icl::discrete_interval<off_t> &range)
{
    messages::fuse::SynchronizeBlockAndComputeChecksum request{
        uuid.toStdString(), range};

    auto syncResponse =
        communicate<messages::fuse::SyncResponse>(std::move(request));

    m_metadataCache.updateLocation(syncResponse.fileLocation());

    return syncResponse.checksum();
}

bool FsLogic::dataCorrupted(const folly::fbstring &uuid,
    const folly::IOBufQueue &buf, const folly::fbstring &serverChecksum,
    const boost::icl::discrete_interval<off_t> &availableRange,
    const boost::icl::discrete_interval<off_t> &wantedRange)
{
    if (availableRange == wantedRange)
        return computeHash(buf) != serverChecksum;

    return syncAndFetchChecksum(uuid, availableRange) != serverChecksum;
}

folly::fbstring FsLogic::computeHash(const folly::IOBufQueue &buf)
{
    // TODO: move this to CPU-bound threadpool
    return folly::fibers::await([&](
        folly::fibers::Promise<folly::fbstring> promise) {
        m_context->scheduler()->post(
            [&, promise = std::move(promise) ]() mutable {
                folly::fbstring hash(MD4_DIGEST_LENGTH, '\0');
                MD4_CTX ctx;
                MD4_Init(&ctx);

                if (!buf.empty())
                    for (auto &byteRange : *buf.front())
                        MD4_Update(&ctx, byteRange.data(), byteRange.size());

                MD4_Final(reinterpret_cast<unsigned char *>(&hash[0]), &ctx);
                promise.setValue(std::move(hash));
            });
    });
}

bool FsLogic::isSpaceDisabled(const folly::fbstring &spaceId)
{
    return m_disabledSpaces.count(spaceId);
}

void FsLogic::disableSpaces(const std::vector<std::string> &spaces)
{
    m_disabledSpaces = {spaces.begin(), spaces.end()};
}

} // namespace fslogic
} // namespace client
} // namespace one
