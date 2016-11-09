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
#include "messages/fuse/deleteFile.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileBlock.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileLocation.h"
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/fileRenamedEntry.h"
#include "messages/fuse/getFileChildren.h"
#include "messages/fuse/getNewFileLocation.h"
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

FsLogic::FsLogic(std::shared_ptr<Context> context,
    std::shared_ptr<messages::Configuration> configuration,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_context{std::move(context)}
    , m_metadataCache{*m_context->communicator()}
    , m_metadataEventHandler{m_eventManager, m_metadataCache, runInFiber}
    , m_helpersCache{*m_context->communicator(), *m_context->scheduler()}
{
    using namespace std::placeholders;

    m_eventManager.setPermissionChangedHandler([=](auto events) {
        runInFiber([ this, events = std::move(events) ] {
            for (auto &event : events)
                m_forceProxyIOCache.handlePermissionChanged(event->fileUuid());
        });
    });

    m_eventManager.setQuotaExeededHandler([=](auto events) {
        runInFiber([ this, events = std::move(events) ] {
            if (!events.empty())
                this->disableSpaces(events.back()->spaces());
        });
    });

    m_eventManager.subscribe(configuration->subscriptionContainer());

    // Quota initial configuration
    m_fsSubscriptions.addQuotaSubscription();
    disableSpaces(configuration->disabledSpacesContainer());

    m_metadataCache.onAdd([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.addFileAttrSubscription(uuid.toStdString());
        m_fsSubscriptions.addFileRemovalSubscription(uuid.toStdString());
        m_fsSubscriptions.addFileRenamedSubscription(uuid.toStdString());
    });

    m_metadataCache.onOpen([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.addFileLocationSubscription(uuid.toStdString());
    });

    m_metadataCache.onPrune([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.removeFileLocationSubscription(uuid.toStdString());
        m_fsSubscriptions.removeFileAttrSubscription(uuid.toStdString());
        m_fsSubscriptions.removeFileRemovalSubscription(uuid.toStdString());
        m_fsSubscriptions.removeFileRenamedSubscription(uuid.toStdString());
    });

    m_metadataCache.onRename([this](
        const folly::fbstring &oldUuid, const folly::fbstring &newUuid) {
        m_fsSubscriptions.removeFileAttrSubscription(oldUuid.toStdString());
        m_fsSubscriptions.removeFileRemovalSubscription(oldUuid.toStdString());
        m_fsSubscriptions.removeFileRenamedSubscription(oldUuid.toStdString());
        m_fsSubscriptions.addFileAttrSubscription(newUuid.toStdString());
        m_fsSubscriptions.addFileRemovalSubscription(newUuid.toStdString());
        m_fsSubscriptions.addFileRenamedSubscription(newUuid.toStdString());

        if (m_fsSubscriptions.removeFileLocationSubscription(
                oldUuid.toStdString()))
            m_fsSubscriptions.addFileLocationSubscription(
                newUuid.toStdString());

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
    auto flagsSet = helpers::maskToFlags(flags);
    auto openFileToken = m_metadataCache.open(uuid, flagsSet);

    const auto filteredFlags = flags & (~O_CREAT) & (~O_APPEND);

    const auto fuseFileHandleId = m_nextFuseHandleId++;
    m_fuseFileHandles.emplace(fuseFileHandleId,
        std::make_shared<FuseFileHandle>(filteredFlags, openFileToken,
                                  m_helpersCache, m_forceProxyIOCache));

    m_eventManager.emitFileOpenedEvent(uuid.toStdString());

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

    if (fuseFileHandle->providerHandleId())
        communicate(messages::fuse::Release{uuid.toStdString(),
            fuseFileHandle->providerHandleId()->toStdString()});

    m_fuseFileHandles.erase(fileHandleId);

    m_eventManager.emitFileReleasedEvent(uuid.toStdString());

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

    auto flagSet = helpers::maskToFlags(fuseFileHandle->flags());

    auto locationData = m_metadataCache.getBlock(uuid, flagSet, offset);
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
        m_eventManager.emitReadEvent(offset, bytesRead, uuid.toStdString());

        return readBuffer;
    }
    catch (const std::system_error &e) {
        if (e.code().value() != EPERM && e.code().value() != EACCES)
            throw;
        if (m_forceProxyIOCache.contains(uuid))
            throw;

        m_forceProxyIOCache.insert(uuid);

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
    auto flagsSet = helpers::maskToFlags(fuseFileHandle->flags());
    auto attr = m_metadataCache.getAttr(uuid);
    auto location = m_metadataCache.getFileLocation(uuid, flagsSet);

    // Check if this space is marked as disabled due to exeeded quota
    if (isSpaceDisabled(location->spaceId()))
        return -ENOSPC;

    messages::fuse::FileBlock fileBlock{
        location->storageId(), location->fileId()};

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

        m_forceProxyIOCache.insert(uuid);
        return write(uuid, fuseFileHandleId, offset, std::move(buf));
    }

    m_eventManager.emitWriteEvent(offset, bytesWritten, uuid.toStdString(),
        fileBlock.storageId(), fileBlock.fileId());

    auto writtenRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + bytesWritten);

    m_metadataCache.addBlock(
        uuid, flagsSet, writtenRange, std::move(fileBlock));

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
    return makeFile(parentUuid, name, mode, helpers::Flag::RDWR);
}

FileAttrPtr FsLogic::makeFile(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const mode_t mode, const helpers::Flag flag)
{
    messages::fuse::GetNewFileLocation msg{parentUuid, name, mode, {flag}};

    auto location = communicate<messages::fuse::FileLocation>(std::move(msg));
    auto uuid = location.uuid();

    m_metadataCache.putLocation(
        flag, std::make_unique<FileLocation>(std::move(location)));

    // TODO: GetNewFileLocation should probably also return attrs
    return m_metadataCache.getAttr(uuid);
}

std::pair<FileAttrPtr, std::uint64_t> FsLogic::create(
    const folly::fbstring &parentUuid, const folly::fbstring &name,
    const mode_t mode, const int flags)
{
    // TODO: create should have its own message instead of combining
    // operations
    const auto flag = cache::getFlagForLocation(helpers::maskToFlags(flags));
    auto attr = makeFile(parentUuid, name, mode, flag);
    auto fh = open(attr->uuid(), flags);
    return {attr, fh};
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
        m_eventManager.emitTruncateEvent(attr.st_size, uuid.toStdString());
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

    m_metadataCache.updateFileLocation(syncResponse.fileLocation());

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
