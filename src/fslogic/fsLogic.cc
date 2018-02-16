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
#include "logging.h"
#include "messages/configuration.h"
#include "messages/fuse/changeMode.h"
#include "messages/fuse/createDir.h"
#include "messages/fuse/createFile.h"
#include "messages/fuse/deleteFile.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileBlock.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileChildrenAttrs.h"
#include "messages/fuse/fileCreated.h"
#include "messages/fuse/fileLocation.h"
#include "messages/fuse/fileOpened.h"
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/fileRenamedEntry.h"
#include "messages/fuse/fsync.h"
#include "messages/fuse/getFileChildren.h"
#include "messages/fuse/getFileChildrenAttrs.h"
#include "messages/fuse/getXAttr.h"
#include "messages/fuse/listXAttr.h"
#include "messages/fuse/makeFile.h"
#include "messages/fuse/openFile.h"
#include "messages/fuse/release.h"
#include "messages/fuse/removeXAttr.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/setXAttr.h"
#include "messages/fuse/syncResponse.h"
#include "messages/fuse/synchronizeBlockAndComputeChecksum.h"
#include "messages/fuse/truncate.h"
#include "messages/fuse/updateTimes.h"
#include "messages/fuse/xattr.h"
#include "messages/fuse/xattrList.h"
#include "monitoring/monitoring.h"

#include <boost/icl/interval_set.hpp>
#include <folly/Enumerate.h>
#include <folly/Range.h>
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
    unsigned int metadataCacheSize, bool readEventsDisabled,
    const std::chrono::seconds providerTimeout,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_context{std::move(context)}
    , m_metadataCache{*m_context->communicator(), metadataCacheSize,
          providerTimeout}
    , m_helpersCache{std::move(helpersCache)}
    , m_readEventsDisabled{readEventsDisabled}
    , m_fsSubscriptions{m_eventManager, m_metadataCache, m_forceProxyIOCache,
          runInFiber}
    , m_providerTimeout{std::move(providerTimeout)}
{
    using namespace std::placeholders;

    m_eventManager.subscribe(*configuration);

    // Quota initial configuration
    m_eventManager.subscribe(
        events::QuotaExceededSubscription{[=](auto events) {
            runInFiber([ this, events = std::move(events) ] {
                this->disableSpaces(events.back()->spaces());
            });
        }});
    disableSpaces(configuration->disabledSpaces());

    m_forceProxyIOCache.onAdd([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.subscribeFilePermChanged(uuid);
    });

    m_forceProxyIOCache.onRemove([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.unsubscribeFilePermChanged(uuid);
    });

    m_metadataCache.onAdd([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.subscribeFileAttrChanged(uuid);
        m_fsSubscriptions.subscribeFileRemoved(uuid);
        m_fsSubscriptions.subscribeFileRenamed(uuid);
    });

    m_metadataCache.onOpen([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.subscribeFileAttrChanged(uuid);
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
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(name);

    return m_metadataCache.getAttr(uuid, name);
}

FileAttrPtr FsLogic::getattr(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    return m_metadataCache.getAttr(uuid);
}

folly::fbvector<folly::fbstring> FsLogic::readdir(
    const folly::fbstring &uuid, const size_t maxSize, const off_t off)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(maxSize) << LOG_FARG(off);

    folly::fbvector<folly::fbstring> acc;
    auto chunkSize = maxSize;

    if (off == 0) {
        // Since the server does not provide '.' and '..' entries, we
        // have to add them here
        if (chunkSize > 0) {
            acc.emplace_back(".");
            chunkSize--;
        }
        if (chunkSize > 0) {
            acc.emplace_back("..");
            chunkSize--;
        }
    }

    if (chunkSize == 0)
        return acc;

    // Since most applications (even plain 'ls') invoke getattr operation
    // after listing directory contents, it is typically faster to
    // fetch them in advance and store them in the cache, thus
    // in fact making this operation equivalent to readdirplus.
    // Adjust the offset in order to account for initial . and .. entries
    LOG_DBG(1) << "Fetching children of directory " << uuid
               << " starting at offset " << off;
    auto msg = communicate<messages::fuse::FileChildrenAttrs>(
        messages::fuse::GetFileChildrenAttrs{
            uuid, off < 2 ? 0 : off - 2, chunkSize},
        m_providerTimeout);

    for (const auto it : folly::enumerate(msg.childrenAttrs())) {
        const auto fileAttrPtr = std::make_shared<FileAttr>(*it);
        m_metadataCache.putAttr(fileAttrPtr);
        acc.emplace_back(fileAttrPtr->name());
    }

    return acc;
}

std::uint64_t FsLogic::open(const folly::fbstring &uuid, const int flags)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARGH(flags);

    auto openFileToken = m_metadataCache.open(uuid);

    const auto filteredFlags = flags & (~O_CREAT) & (~O_APPEND);

    const auto flag = getOpenFlag(helpers::maskToFlags(filteredFlags));
    messages::fuse::OpenFile msg{uuid.toStdString(), flag};

    LOG_DBG(1) << "Sending file opened message for " << uuid;

    auto opened = communicate<messages::fuse::FileOpened>(
        std::move(msg), m_providerTimeout);

    const auto fuseFileHandleId = m_nextFuseHandleId++;
    m_fuseFileHandles.emplace(fuseFileHandleId,
        std::make_shared<FuseFileHandle>(filteredFlags, opened.handleId(),
            openFileToken, *m_helpersCache, m_forceProxyIOCache,
            m_providerTimeout));

    LOG_DBG(1) << "Stored fuse handle for file " << uuid;

    return fuseFileHandleId;
}

void FsLogic::release(
    const folly::fbstring &uuid, const std::uint64_t fileHandleId)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fileHandleId);

    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);

    fsync(uuid, fileHandleId, false);

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
        communication::wait(releaseExceptionFuture, m_providerTimeout);
    }
    catch (...) {
        releaseException = std::current_exception();
    }

    LOG_DBG(1) << "Sending file release message for " << uuid;

    communicate(messages::fuse::Release{uuid.toStdString(),
                    fuseFileHandle->providerHandleId()->toStdString()},
        m_providerTimeout);

    m_fuseFileHandles.erase(fileHandleId);

    if (releaseException)
        std::rethrow_exception(releaseException);
}

void FsLogic::flush(
    const folly::fbstring &uuid, const std::uint64_t fileHandleId)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fileHandleId);

    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);

    LOG_DBG(1) << "Sending file flush message for " << uuid;

    for (auto &helperHandle : fuseFileHandle->helperHandles())
        communication::wait(helperHandle->flush(), helperHandle->timeout());
}

void FsLogic::fsync(const folly::fbstring &uuid,
    const std::uint64_t fileHandleId, const bool dataOnly)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fileHandleId)
                << LOG_FARG(dataOnly);

    m_eventManager.flush();

    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);

    LOG_DBG(1) << "Sending file fsync message for " << uuid;

    communicate(messages::fuse::FSync{uuid.toStdString(), dataOnly,
                    fuseFileHandle->providerHandleId()->toStdString()},
        m_providerTimeout);
    for (auto &helperHandle : fuseFileHandle->helperHandles())
        communication::wait(
            helperHandle->fsync(dataOnly), helperHandle->timeout());
}

folly::IOBufQueue FsLogic::read(const folly::fbstring &uuid,
    const std::uint64_t fileHandleId, const off_t offset,
    const std::size_t size, folly::Optional<folly::fbstring> checksum)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fileHandleId) << LOG_FARG(offset)
                << LOG_FARG(size);

    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);
    auto attr = m_metadataCache.getAttr(uuid);

    const auto possibleRange =
        boost::icl::discrete_interval<off_t>::right_open(0, *attr->size());

    const auto requestedRange =
        boost::icl::discrete_interval<off_t>::right_open(offset, offset + size);

    const auto wantedRange = requestedRange & possibleRange;

    if (boost::icl::size(wantedRange) == 0) {
        LOG_DBG(1) << "Read requested for impossible range " << requestedRange
                   << " for file " << uuid;
        return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
    }

    LOG_DBG(1) << "Reading from file " << uuid << " from range " << wantedRange;

    // Even if several "touching" blocks with different helpers are
    // available to read right now, for simplicity we'll only read a single
    // block per a read operation.

    auto locationData = m_metadataCache.getBlock(uuid, offset);
    if (!locationData.hasValue()) {
        auto csum = syncAndFetchChecksum(uuid, wantedRange);
        LOG_DBG(1) << "Requested block for " << uuid
                   << " not in cache - fetching from server";
        return read(uuid, fileHandleId, offset, size, std::move(csum));
    }

    boost::icl::discrete_interval<off_t> availableRange;
    messages::fuse::FileBlock fileBlock;
    std::tie(availableRange, fileBlock) = std::move(*locationData);
    availableRange = availableRange & wantedRange;

    LOG_DBG(1) << "Availble block range for file " << uuid
               << " in requested range: " << availableRange;

    const std::size_t availableSize = boost::icl::size(availableRange);

    try {
        auto helperHandle = fuseFileHandle->getHelperHandle(uuid,
            m_metadataCache.getSpaceId(uuid), fileBlock.storageId(),
            fileBlock.fileId());

        if (checksum) {
            LOG_DBG(1) << "Waiting on helper flush for " << uuid
                       << " due to required checksum";
            communication::wait(helperHandle->flush(), helperHandle->timeout());
        }

        LOG_DBG(1) << "Reading " << availableSize << " bytes from " << uuid
                   << " at offset " << offset;
        auto readBuffer = communication::wait(
            helperHandle->read(offset, availableSize), helperHandle->timeout());

        if (helperHandle->needsDataConsistencyCheck() && checksum &&
            dataCorrupted(
                uuid, readBuffer, *checksum, availableRange, wantedRange)) {

            // close the file to get data up to date, it will be opened
            // again by read function
            fuseFileHandle->releaseHelperHandle(
                uuid, fileBlock.storageId(), fileBlock.fileId());

            LOG_DBG(1) << "Rereading the requested block from file " << uuid
                       << " due to mismatch in checksum";

            return read(uuid, fileHandleId, offset, size, checksum);
        }

        const auto bytesRead = readBuffer.chainLength();
        if (!m_readEventsDisabled) {
            m_eventManager.emit<events::FileRead>(
                uuid.toStdString(), offset, bytesRead);
        }

        LOG_DBG(1) << "Read " << bytesRead << " bytes from " << uuid
                   << " at offset " << offset;

        return readBuffer;
    }
    catch (const std::system_error &e) {
        if (e.code().value() != EPERM && e.code().value() != EACCES) {
            LOG(ERROR) << "Reading from " << uuid
                       << " failed to insufficient permissions";
            throw;
        }
        if (m_forceProxyIOCache.contains(uuid)) {
            LOG(ERROR) << "Reading from " << uuid
                       << " failed since proxy mode is forced for this file";
            throw;
        }

        LOG_DBG(1) << "Adding file " << uuid
                   << " to force proxy cache after direct read failed";
        m_forceProxyIOCache.add(uuid);

        LOG_DBG(1) << "Rereading requested block for " << uuid
                   << " via proxy fallback";
        return read(uuid, fileHandleId, offset, size, checksum);
    }
}

std::size_t FsLogic::write(const folly::fbstring &uuid,
    const std::uint64_t fuseFileHandleId, const off_t offset,
    folly::IOBufQueue buf)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fuseFileHandleId)
                << LOG_FARG(offset) << LOG_FARG(buf.chainLength());

    if (buf.empty()) {
        LOG_DBG(1) << "Write called with empty buffer - skipping";
        return 0;
    }

    auto fuseFileHandle = m_fuseFileHandles.at(fuseFileHandleId);
    auto attr = m_metadataCache.getAttr(uuid);
    auto spaceId = m_metadataCache.getSpaceId(uuid);

    // Check if this space is marked as disabled due to exeeded quota
    if (isSpaceDisabled(spaceId)) {
        LOG(ERROR) << "Write to file " << uuid << " failed - space "
                   << m_metadataCache.getSpaceId(uuid) << " quota exceeded";
        throw std::errc::no_space_on_device;
    }

    auto fileBlock = m_metadataCache.getDefaultBlock(uuid);

    size_t bytesWritten = 0;
    try {
        auto helperHandle = fuseFileHandle->getHelperHandle(
            uuid, spaceId, fileBlock.storageId(), fileBlock.fileId());

        bytesWritten =
            communication::wait(helperHandle->write(offset, std::move(buf)),
                helperHandle->timeout());
    }
    catch (const std::system_error &e) {
        if (e.code().value() != EPERM && e.code().value() != EACCES) {
            LOG(ERROR) << "Reading from " << uuid
                       << " failed with error code: " << e.what();
            throw;
        }

        if (m_forceProxyIOCache.contains(uuid)) {
            LOG(ERROR) << "Reading from " << uuid
                       << " failed since proxy mode is forced for this file";
            throw;
        }

        LOG_DBG(1) << "Adding file " << uuid
                   << " to force proxy cache after direct read failed";

        m_forceProxyIOCache.add(uuid);

        LOG_DBG(1) << "Writing requested block for " << uuid
                   << " via proxy fallback";

        return write(uuid, fuseFileHandleId, offset, std::move(buf));
    }

    m_eventManager.emit<events::FileWritten>(uuid.toStdString(), offset,
        bytesWritten, fileBlock.storageId(), fileBlock.fileId());

    auto writtenRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + bytesWritten);

    LOG_DBG(1) << "Written " << bytesWritten << " bytes to file " << uuid
               << " at offset " << offset << " on storage "
               << fileBlock.storageId();

    m_metadataCache.addBlock(uuid, writtenRange, std::move(fileBlock));

    return bytesWritten;
}

FileAttrPtr FsLogic::mkdir(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const mode_t mode)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name) << LOG_FARG(mode);

    // TODO: CreateDir should probably also return attrs
    communicate(messages::fuse::CreateDir{parentUuid.toStdString(),
                    name.toStdString(), mode},
        m_providerTimeout);

    LOG_DBG(1) << "Created directory " << name << " in " << parentUuid;

    // TODO: Provider returns uuid of the created dir, no need for lookup
    return m_metadataCache.getAttr(parentUuid, name);
}

FileAttrPtr FsLogic::mknod(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const mode_t mode)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name) << LOG_FARG(mode);

    messages::fuse::MakeFile msg{parentUuid, name, mode};
    auto attr = communicate<FileAttr>(std::move(msg), m_providerTimeout);
    auto sharedAttr = std::make_shared<FileAttr>(std::move(attr));
    m_metadataCache.putAttr(sharedAttr);

    LOG_DBG(1) << "Created node " << name << " in " << parentUuid
               << " with uuid " << attr.uuid();

    return sharedAttr;
}

std::pair<FileAttrPtr, std::uint64_t> FsLogic::create(
    const folly::fbstring &parentUuid, const folly::fbstring &name,
    const mode_t mode, const int flags)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name) << LOG_FARG(mode)
                << LOG_FARG(flags);

    const auto flag = getOpenFlag(helpers::maskToFlags(flags));
    messages::fuse::CreateFile msg{parentUuid, name, mode, flag};

    auto created = communicate<messages::fuse::FileCreated>(
        std::move(msg), m_providerTimeout);

    const auto &uuid = created.attr().uuid();
    auto sharedAttr = std::make_shared<FileAttr>(std::move(created.attr()));
    auto location = std::make_unique<FileLocation>(created.location());
    auto openFileToken =
        m_metadataCache.open(uuid, sharedAttr, std::move(location));

    const auto fuseFileHandleId = m_nextFuseHandleId++;
    m_fuseFileHandles.emplace(fuseFileHandleId,
        std::make_shared<FuseFileHandle>(flags, created.handleId(),
            openFileToken, *m_helpersCache, m_forceProxyIOCache,
            m_providerTimeout));

    LOG_DBG(1) << "Created file " << name << " in " << parentUuid
               << " with uuid " << uuid;

    return {sharedAttr, fuseFileHandleId};
}

void FsLogic::unlink(
    const folly::fbstring &parentUuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name);

    // TODO: directly order provider to delete {parentUuid, name}
    auto attr = m_metadataCache.getAttr(parentUuid, name);
    communicate(messages::fuse::DeleteFile{attr->uuid().toStdString()},
        m_providerTimeout);
    m_metadataCache.markDeleted(attr->uuid());

    LOG_DBG(1) << "Deleted file " << name << " in " << parentUuid
               << " with uuid " << attr->uuid();
}

void FsLogic::rename(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const folly::fbstring &newParentUuid,
    const folly::fbstring &newName)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name)
                << LOG_FARG(newParentUuid) << LOG_FARG(newName);

    // TODO: directly order provider to rename {parentUuid, name}
    auto attr = m_metadataCache.getAttr(parentUuid, name);
    auto oldUuid = attr->uuid();

    auto renamed = communicate<messages::fuse::FileRenamed>(
        messages::fuse::Rename{oldUuid.toStdString(),
            newParentUuid.toStdString(), newName.toStdString()},
        m_providerTimeout);

    m_metadataCache.rename(oldUuid, newParentUuid, newName, renamed.newUuid());

    LOG_DBG(1) << "Renamed file " << name << " in " << parentUuid << " to "
               << newName << " in " << newParentUuid;

    for (auto &child : renamed.childEntries())
        m_metadataCache.rename(child.oldUuid(), child.newParentUuid(),
            child.newName(), child.newUuid());
}

FileAttrPtr FsLogic::setattr(
    const folly::fbstring &uuid, const struct stat &attr, const int toSet)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(toSet);

    // TODO: this operation can be optimized with a single message to the
    // provider

    if (toSet & FUSE_SET_ATTR_UID || toSet & FUSE_SET_ATTR_GID) {
        LOG_DBG(1) << "Attempting to modify uid or gid attempted for " << uuid
                   << ". Operation not supported.";
        throw std::errc::operation_not_supported;
    }

    if (toSet & FUSE_SET_ATTR_MODE) {
        // ALLPERMS is a macro of sys/stat.h
        const mode_t normalizedMode = attr.st_mode & ALLPERMS;

        communicate(
            messages::fuse::ChangeMode{uuid.toStdString(), normalizedMode},
            m_providerTimeout);

        m_metadataCache.changeMode(uuid, normalizedMode);

        LOG_DBG(1) << "Changed mode of " << uuid << " to "
                   << LOG_OCT(normalizedMode);
    }

    if (toSet & FUSE_SET_ATTR_SIZE) {
        communicate(messages::fuse::Truncate{uuid.toStdString(), attr.st_size},
            m_providerTimeout);
        m_metadataCache.truncate(uuid, attr.st_size);
        m_eventManager.emit<events::FileTruncated>(
            uuid.toStdString(), attr.st_size);

        LOG_DBG(1) << "Truncated file " << uuid << " to size " << attr.st_size
                   << " via setattr";

        ONE_METRIC_COUNTER_INC(
            "comp.oneclient.mod.events.submod.emitted.truncate");
    }

    messages::fuse::UpdateTimes updateTimes{uuid.toStdString()};

    const auto now = std::chrono::system_clock::now();
    updateTimes.ctime(now);
    if (toSet & FUSE_SET_ATTR_ATIME) {
        updateTimes.atime(
            std::chrono::system_clock::from_time_t(attr.st_atime));
        LOG_DBG(1) << "Changed atime of " << uuid << " to " << attr.st_atime;
    }
    if (toSet & FUSE_SET_ATTR_MTIME) {
        updateTimes.mtime(
            std::chrono::system_clock::from_time_t(attr.st_mtime));
        LOG_DBG(1) << "Changed mtime of " << uuid << " to " << attr.st_atime;
    }
#if defined(FUSE_SET_ATTR_ATIME_NOW)
    if (toSet & FUSE_SET_ATTR_ATIME_NOW) {
        updateTimes.atime(now);
        LOG_DBG(1) << "Changed atime of " << uuid << " to now";
    }
#endif
#if defined(FUSE_SET_ATTR_MTIME_NOW)
    if (toSet & FUSE_SET_ATTR_MTIME_NOW) {
        updateTimes.mtime(now);
        LOG_DBG(1) << "Changed mtime of " << uuid << " to now";
    }
#endif

    communicate(updateTimes, m_providerTimeout);
    m_metadataCache.updateTimes(uuid, updateTimes);

    return m_metadataCache.getAttr(uuid);
}

folly::fbstring FsLogic::getxattr(
    const folly::fbstring &uuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(name);

    folly::fbstring result;

    messages::fuse::GetXAttr getXAttrRequest{uuid, name};
    auto xattr =
        communicate<messages::fuse::XAttr>(getXAttrRequest, m_providerTimeout);
    result = xattr.value();

    LOG_DBG(1) << "Received xattr " << name << " value for file " << uuid;

    return result;
}

void FsLogic::setxattr(const folly::fbstring &uuid, const folly::fbstring &name,
    const folly::fbstring &value, bool create, bool replace)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(name) << LOG_FARG(value)
                << LOG_FARG(create) << LOG_FARG(replace);

    messages::fuse::SetXAttr setXAttrRequest{
        uuid, name, value, create, replace};
    communicate<messages::fuse::FuseResponse>(
        setXAttrRequest, m_providerTimeout);

    LOG_DBG(1) << "Set xattr " << name << " value for file " << uuid;
}

void FsLogic::removexattr(
    const folly::fbstring &uuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(name);

    messages::fuse::RemoveXAttr removeXAttrRequest{uuid, name};
    communicate<messages::fuse::FuseResponse>(
        removeXAttrRequest, m_providerTimeout);

    LOG_DBG(1) << "Removed xattr " << name << " from file " << uuid;
}

folly::fbvector<folly::fbstring> FsLogic::listxattr(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    folly::fbvector<folly::fbstring> result;

    messages::fuse::ListXAttr listXAttrRequest{uuid};
    messages::fuse::XAttrList fuseResponse =
        communicate<messages::fuse::XAttrList>(
            listXAttrRequest, m_providerTimeout);

    for (const auto &xattrName : fuseResponse.xattrNames()) {
        result.push_back(xattrName.c_str());
    }

    LOG_DBG(1) << "Received xattr list for file " << uuid;

    return result;
}

template <typename SrvMsg, typename CliMsg>
SrvMsg FsLogic::communicate(CliMsg &&msg, const std::chrono::seconds timeout)
{
    return communication::wait(m_context->communicator()->communicate<SrvMsg>(
                                   std::forward<CliMsg>(msg)),
        timeout);
}

folly::fbstring FsLogic::syncAndFetchChecksum(const folly::fbstring &uuid,
    const boost::icl::discrete_interval<off_t> &range)
{
    messages::fuse::SynchronizeBlockAndComputeChecksum request{
        uuid.toStdString(), range};

    auto syncResponse = communicate<messages::fuse::SyncResponse>(
        std::move(request), m_providerTimeout);

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
    LOG_FCALL() << LOG_FARG(buf.chainLength());
    // TODO: move this to CPU-bound threadpool
    return folly::fibers::await(
        [&](folly::fibers::Promise<folly::fbstring> promise) {
            m_context->scheduler()->post(
                [&, promise = std::move(promise) ]() mutable {
                    folly::fbstring hash(MD4_DIGEST_LENGTH, '\0');
                    MD4_CTX ctx;
                    MD4_Init(&ctx);

                    if (!buf.empty())
                        for (auto &byteRange : *buf.front())
                            MD4_Update(
                                &ctx, byteRange.data(), byteRange.size());

                    MD4_Final(
                        reinterpret_cast<unsigned char *>(&hash[0]), &ctx);
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
