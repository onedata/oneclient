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
#include "messages/fuse/fileLocationChanged.h"
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
#include "messages/fuse/synchronizeBlock.h"
#include "messages/fuse/synchronizeBlockAndComputeChecksum.h"
#include "messages/fuse/truncate.h"
#include "messages/fuse/updateTimes.h"
#include "messages/fuse/xattr.h"
#include "messages/fuse/xattrList.h"
#include "monitoring/monitoring.h"
#include "util/cdmi.h"

#include <boost/icl/interval_set.hpp>
#include <folly/Enumerate.h>
#include <folly/Range.h>
#include <folly/ScopeGuard.h>
#include <folly/fibers/Baton.h>
#include <folly/fibers/FiberManager.h>
#include <folly/fibers/ForEach.h>
#include <folly/json.h>
#include <fuse/fuse_lowlevel.h>
#include <openssl/md4.h>

#define IOTRACE_START() auto __ioTraceStart = std::chrono::system_clock::now();

#define IOTRACE_END(TraceType, optype, uuid, handleId, ...)                    \
    if (m_ioTraceLoggerEnabled)                                                \
        m_ioTraceLogger->log(TraceType(__ioTraceStart, optype,                 \
            std::chrono::duration_cast<std::chrono::microseconds>(             \
                std::chrono::system_clock::now() - __ioTraceStart),            \
            uuid, handleId, 0, ##__VA_ARGS__));

#define IOTRACE_GUARD(TraceType, optype, uuid, handleId, ...)                  \
    IOTRACE_START()                                                            \
    auto __ioTraceGuard = folly::makeGuard([&] {                               \
        IOTRACE_END(TraceType, optype, uuid, handleId, ##__VA_ARGS__)          \
    });

namespace one {
namespace client {
namespace fslogic {
using namespace std::literals;

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

constexpr auto XATTR_FILE_BLOCKS_MAP_LENGTH = 50;

inline static folly::fbstring ONE_XATTR(std::string name)
{
    assert(name.size() > 0);
    return ONE_XATTR_PREFIX + name;
}

FsLogic::FsLogic(std::shared_ptr<Context> context,
    std::shared_ptr<messages::Configuration> configuration,
    std::unique_ptr<cache::HelpersCache> helpersCache,
    unsigned int metadataCacheSize, bool readEventsDisabled,
    bool forceFullblockRead, const std::chrono::seconds providerTimeout,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_context{context}
    , m_metadataCache{*m_context->communicator(), metadataCacheSize,
          providerTimeout}
    , m_helpersCache{std::move(helpersCache)}
    , m_readdirCache{std::make_shared<cache::ReaddirCache>(
          m_metadataCache, m_context)}
    , m_readEventsDisabled{readEventsDisabled}
    , m_forceFullblockRead{forceFullblockRead}
    , m_fsSubscriptions{m_eventManager, m_metadataCache, m_forceProxyIOCache,
          runInFiber}
    , m_nextFuseHandleId{0}
    , m_providerTimeout{std::move(providerTimeout)}
    , m_runInFiber{std::move(runInFiber)} /* clang-format off */
    , m_linearReadPrefetchThreshold{m_context->options()
          ->getLinearReadPrefetchThreshold()}
    , m_randomReadPrefetchThreshold{m_context->options()
          ->getRandomReadPrefetchThreshold()}
    , m_randomReadPrefetchBlockThreshold{m_context->options()
          ->getRandomReadPrefetchBlockThreshold()}
    , m_randomReadPrefetchClusterWindow{m_context->options()
          ->getRandomReadPrefetchClusterWindow()}
    , m_randomReadPrefetchClusterBlockThreshold{m_context->options()
          ->getRandomReadPrefetchClusterBlockThreshold()}
    , m_randomReadPrefetchClusterWindowGrowFactor{m_context->options()
          ->getRandomReadPrefetchClusterWindowGrowFactor()}
    , m_clusterPrefetchThresholdRandom{m_context->options()
          ->isClusterPrefetchThresholdRandom()}
    , m_ioTraceLoggerEnabled{m_context->options()->isIOTraceLoggerEnabled()}
/* clang-format on */
{
    using namespace std::placeholders;

    m_nextFuseHandleId = 0;

    m_eventManager.subscribe(*configuration);

    m_metadataCache.setReaddirCache(m_readdirCache);

    // Quota initial configuration
    m_eventManager.subscribe(
        events::QuotaExceededSubscription{[=](auto events) {
            m_runInFiber([ this, events = std::move(events) ] {
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

    m_metadataCache.onRelease([this](const folly::fbstring &uuid) {
        m_fsSubscriptions.unsubscribeFileLocationChanged(uuid);
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

    if (m_clusterPrefetchThresholdRandom) {
        m_clusterPrefetchDistribution = std::uniform_int_distribution<int>(
            2, m_randomReadPrefetchClusterBlockThreshold);
    }

    if (m_ioTraceLoggerEnabled) {
        m_ioTraceLogger = createIOTraceLogger();
        IOTRACE_GUARD(IOTraceMount, IOTraceLogger::OpType::MOUNT,
            configuration->rootUuid(), 0,
            context->options()->getMountpoint().string());
    }
}

FileAttrPtr FsLogic::lookup(
    const folly::fbstring &uuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(name);

    IOTRACE_START()

    auto attr = m_metadataCache.getAttr(uuid, name);

    auto type = attr->type() == FileAttr::FileType::directory ? "d" : "f";
    auto size = attr->size();

    IOTRACE_END(IOTraceLookup, IOTraceLogger::OpType::LOOKUP, uuid, 0, name,
        attr->uuid(), type, size ? *size : 0);

    return attr;
}

FileAttrPtr FsLogic::getattr(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    IOTRACE_GUARD(IOTraceGetAttr, IOTraceLogger::OpType::GETATTR, uuid, 0)

    return m_metadataCache.getAttr(uuid);
}

folly::fbvector<folly::fbstring> FsLogic::readdir(
    const folly::fbstring &uuid, const size_t maxSize, const off_t off)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(maxSize) << LOG_FARG(off);

    IOTRACE_GUARD(
        IOTraceReadDir, IOTraceLogger::OpType::READDIR, uuid, 0, maxSize, off)

    return m_readdirCache->readdir(uuid, off, maxSize);
}

std::uint64_t FsLogic::open(const folly::fbstring &uuid, const int flags)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARGH(flags);

    IOTRACE_START()

    auto openFileToken = m_metadataCache.open(uuid);

    const auto filteredFlags = flags & (~O_CREAT) & (~O_APPEND);

    const auto flag = getOpenFlag(helpers::maskToFlags(filteredFlags));
    messages::fuse::OpenFile msg{uuid.toStdString(), flag};

    LOG_DBG(2) << "Sending file opened message for " << uuid;

    auto opened = communicate<messages::fuse::FileOpened>(
        std::move(msg), m_providerTimeout);

    const auto fuseFileHandleId = m_nextFuseHandleId++;

    m_fuseFileHandles.emplace(fuseFileHandleId,
        std::make_shared<FuseFileHandle>(filteredFlags, opened.handleId(),
            openFileToken, *m_helpersCache, m_forceProxyIOCache,
            m_providerTimeout));

    IOTRACE_END(
        IOTraceOpen, IOTraceLogger::OpType::OPEN, uuid, fuseFileHandleId, flags)

    LOG_DBG(2) << "Assigned fuse handle " << fuseFileHandleId << " for file "
               << uuid;

    return fuseFileHandleId;
}

void FsLogic::release(
    const folly::fbstring &uuid, const std::uint64_t fileHandleId)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fileHandleId);

    IOTRACE_GUARD(
        IOTraceRelease, IOTraceLogger::OpType::RELEASE, uuid, fileHandleId)

    if (m_fuseFileHandles.find(fileHandleId) == m_fuseFileHandles.cend()) {
        LOG_DBG(1) << "Fuse file handle " << fileHandleId
                   << " already released.";
        return;
    }

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
    catch (const std::exception &e) {
        LOG(WARNING) << "File release failed: " << e.what();
        releaseException = std::current_exception();
    }
    catch (...) {
        LOG(WARNING) << "File release failed: unknown error";
        releaseException = std::current_exception();
    }

    LOG_DBG(2) << "Sending file release message for " << uuid;

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

    IOTRACE_GUARD(
        IOTraceFlush, IOTraceLogger::OpType::FLUSH, uuid, fileHandleId)

    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);

    LOG_DBG(2) << "Sending file flush message for " << uuid;

    for (auto &helperHandle : fuseFileHandle->helperHandles())
        communication::wait(helperHandle->flush(), helperHandle->timeout());
}

void FsLogic::fsync(const folly::fbstring &uuid,
    const std::uint64_t fileHandleId, const bool dataOnly)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fileHandleId)
                << LOG_FARG(dataOnly);

    IOTRACE_GUARD(IOTraceFsync, IOTraceLogger::OpType::FSYNC, uuid,
        fileHandleId, dataOnly)

    m_eventManager.flush();

    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);

    LOG_DBG(2) << "Sending file fsync message for " << uuid;

    communicate(messages::fuse::FSync{uuid.toStdString(), dataOnly,
                    fuseFileHandle->providerHandleId()->toStdString()},
        m_providerTimeout);

    for (auto &helperHandle : fuseFileHandle->helperHandles())
        communication::wait(
            helperHandle->fsync(dataOnly), helperHandle->timeout());
}

folly::IOBufQueue FsLogic::read(const folly::fbstring &uuid,
    const std::uint64_t fileHandleId, const off_t offset,
    const std::size_t size, folly::Optional<folly::fbstring> checksum,
    const int retriesLeft, std::unique_ptr<IOTraceRead> ioTraceEntry)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fileHandleId) << LOG_FARG(offset)
                << LOG_FARG(size);

    if (m_ioTraceLoggerEnabled && !ioTraceEntry) {
        ioTraceEntry = std::make_unique<IOTraceRead>();
        ioTraceEntry->opType = IOTraceLogger::OpType::READ;
        ioTraceEntry->uuid = uuid;
        ioTraceEntry->handleId = fileHandleId;
        ioTraceEntry->retries = 0;
        std::get<0>(ioTraceEntry->arguments) = offset;
        std::get<1>(ioTraceEntry->arguments) = size;
        std::get<2>(ioTraceEntry->arguments) = true;
        std::get<3>(ioTraceEntry->arguments) = 0;
        std::get<4>(ioTraceEntry->arguments) =
            IOTraceLogger::toString(IOTraceLogger::PrefetchType::NONE);
    }

    auto fuseFileHandle = m_fuseFileHandles.at(fileHandleId);
    auto attr = m_metadataCache.getAttr(uuid);

    const auto fileSize = *attr->size();
    const auto possibleRange =
        boost::icl::discrete_interval<off_t>::right_open(0, fileSize);

    const auto requestedRange =
        boost::icl::discrete_interval<off_t>::right_open(offset, offset + size);

    auto wantedRange = requestedRange & possibleRange;

    if (boost::icl::size(wantedRange) == 0) {
        LOG_DBG(2) << "Read requested for impossible range " << requestedRange
                   << " for file " << uuid;
        return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
    }

    LOG_DBG(2) << "Reading from file " << uuid << " from range " << wantedRange;

    // Even if several "touching" blocks with different helpers are
    // available to read right now, for simplicity we'll only read a single
    // block per a read operation.
    try {
        auto locationData = m_metadataCache.getBlock(uuid, offset);
        if (!locationData.hasValue()) {
            LOG_DBG(2) << "Requested block for " << uuid
                       << " not yet replicated - fetching from remote provider";

            auto defaultBlock = m_metadataCache.getDefaultBlock(uuid);
            auto helperHandle = fuseFileHandle->getHelperHandle(uuid,
                m_metadataCache.getSpaceId(uuid), defaultBlock.storageId(),
                defaultBlock.fileId());

            folly::Optional<folly::fbstring> csum;
            if (helperHandle->needsDataConsistencyCheck())
                csum = syncAndFetchChecksum(uuid, wantedRange);
            else
                sync(uuid, wantedRange);

            if (m_ioTraceLoggerEnabled)
                std::get<2>(ioTraceEntry->arguments) = false;

            return read(uuid, fileHandleId, offset, size, std::move(csum),
                retriesLeft, std::move(ioTraceEntry));
        }

        boost::icl::discrete_interval<off_t> availableRange;
        messages::fuse::FileBlock fileBlock;
        std::tie(availableRange, fileBlock) = std::move(*locationData);
        const auto wantedAvailableRange = availableRange & wantedRange;

        LOG_DBG(2) << "Available block range for file " << uuid
                   << " in requested range: " << wantedAvailableRange;

        const std::size_t availableSize =
            boost::icl::size(wantedAvailableRange);

        auto helperHandle = fuseFileHandle->getHelperHandle(uuid,
            m_metadataCache.getSpaceId(uuid), fileBlock.storageId(),
            fileBlock.fileId());

        if (checksum) {
            LOG_DBG(1) << "Waiting on helper flush for " << uuid
                       << " due to required checksum";
            communication::wait(
                helperHandle->flushUnderlying(), helperHandle->timeout());
        }

        auto prefetchParams = prefetchAsync(fuseFileHandle, helperHandle,
            offset, availableSize, uuid, possibleRange, availableRange);

        if (m_ioTraceLoggerEnabled) {
            std::get<3>(ioTraceEntry->arguments) = prefetchParams.first;
            std::get<4>(ioTraceEntry->arguments) =
                IOTraceLogger::toString(prefetchParams.second);
        }

        const std::size_t continuousSize =
            boost::icl::size(boost::icl::left_subtract(availableRange,
                boost::icl::discrete_interval<off_t>::right_open(0, offset)));

        LOG_DBG(2) << "Reading " << availableSize << " bytes from " << uuid
                   << " at offset " << offset;

        auto readBuffer = communication::wait(
            helperHandle->read(offset, availableSize, continuousSize),
            helperHandle->timeout());

        if (helperHandle->needsDataConsistencyCheck() && checksum &&
            dataCorrupted(uuid, readBuffer, *checksum, wantedAvailableRange,
                wantedRange)) {
            // close the file to get data up to date, it will be opened
            // again by read function
            fuseFileHandle->releaseHelperHandle(
                uuid, fileBlock.storageId(), fileBlock.fileId());

            LOG_DBG(1) << "Rereading the requested block from file " << uuid
                       << " due to mismatch in checksum";

            if (retriesLeft) {
                fiberRetryDelay(retriesLeft);
                LOG_DBG(1) << "Retrying read of " << size << " bytes at offset "
                           << offset << " from file " << uuid
                           << " - invalid checksum";

                if (m_ioTraceLoggerEnabled)
                    ioTraceEntry->retries++;

                return read(uuid, fileHandleId, offset, size, checksum,
                    retriesLeft - 1, std::move(ioTraceEntry));
            }
            else {
                LOG(ERROR) << "Failed to read " << size << " bytes at offset "
                           << offset << " from file " << uuid
                           << " - invalid checksum";
                throw std::system_error(
                    std::make_error_code(std::errc::io_error));
            }
        }

        const auto bytesRead = readBuffer.chainLength();
        if (!m_readEventsDisabled) {
            m_eventManager.emit<events::FileRead>(
                uuid.toStdString(), offset, bytesRead);
        }

        LOG_DBG(2) << "Read " << bytesRead << " bytes from " << uuid
                   << " at offset " << offset;

        if (m_ioTraceLoggerEnabled) {
            using namespace std::chrono;
            std::get<1>(ioTraceEntry->arguments) = bytesRead;
            ioTraceEntry->duration = duration_cast<microseconds>(
                system_clock::now() - ioTraceEntry->timestamp);
            m_ioTraceLogger->log(*ioTraceEntry);
        }

        return readBuffer;
    }
    catch (const std::system_error &e) {
        if (e.code().value() == EAGAIN && retriesLeft) {
            fiberRetryDelay(retriesLeft);
            return read(uuid, fileHandleId, offset, size, checksum,
                retriesLeft - 1, std::move(ioTraceEntry));
        }
        else if (e.code().value() != EPERM && e.code().value() != EACCES) {
            LOG_DBG(1) << "Reading from " << uuid
                       << " failed due to error: " << e.what() << "("
                       << e.code() << ")";
            throw;
        }
        else {
            LOG(ERROR) << "Reading from " << uuid
                       << " failed due to insufficient permissions";
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

        return read(uuid, fileHandleId, offset, size, checksum,
            FSLOGIC_RETRY_COUNT, std::move(ioTraceEntry));
    }
}

std::pair<size_t, IOTraceLogger::PrefetchType> FsLogic::prefetchAsync(
    std::shared_ptr<FuseFileHandle> fuseFileHandle,
    helpers::FileHandlePtr helperHandle, const off_t offset,
    const std::size_t size, const folly::fbstring &uuid,
    const boost::icl::discrete_interval<off_t> possibleRange,
    const boost::icl::discrete_interval<off_t> availableRange)
{
    const auto fileSize = m_metadataCache.getAttr(uuid)->size().value_or(0);
    const auto fileLocation = m_metadataCache.getLocation(uuid);
    const auto replicationProgress =
        fileLocation->replicationProgress(fileSize);

    size_t prefetchSize = 0;
    auto prefetchType = IOTraceLogger::PrefetchType::NONE;

    if (replicationProgress == 1.0) {
        return {prefetchSize, prefetchType};
    }

    const std::size_t wouldPrefetch = helperHandle->wouldPrefetch(offset, size);

    const auto wantToPrefetchRange =
        boost::icl::discrete_interval<off_t>::right_open(
            offset + size, offset + size + wouldPrefetch * 2);

    boost::icl::discrete_interval<off_t> prefetchRange{};
    bool worthPrefetching = false;
    bool fullFilePrefetchRequested = false;
    bool clusterPrefetchRequested = false;
    int prefetchPriority = SYNCHRONIZE_BLOCK_PRIORITY_DEFAULT;

    // Check if we should consider full file prefetch
    if (((m_randomReadPrefetchBlockThreshold &&
             fileLocation->blocksCount() >
                 m_randomReadPrefetchBlockThreshold) ||
            (m_linearReadPrefetchThreshold < 1.0 &&
                fileLocation->linearReadPrefetchThresholdReached(
                    m_linearReadPrefetchThreshold, fileSize)) ||
            (m_randomReadPrefetchThreshold < 1.0 &&
                fileLocation->randomReadPrefetchThresholdReached(
                    m_randomReadPrefetchThreshold, fileSize)))) {

        if (!fuseFileHandle->fullPrefetchTriggered() &&
            m_context->options()->isPrefetchModeAsynchronous() &&
            m_context->options()->getAccessToken()) {
            using namespace one::client::util::cdmi;

            auto p = std::make_shared<folly::Promise<std::string>>();
            auto f = p->getFuture();

            m_context->scheduler()->post(
                [ this, uuid = uuid, p = std::move(p) ]() mutable {
                    p->setValue(m_context->communicator()->makeHttpRequest(
                        m_context->options()->getAccessToken().get(), "POST",
                        "/api/v3/oneprovider/replicas-id/" +
                            uuidToObjectId(uuid.toStdString()),
                        "", ""));
                });

            f.then([uuid = uuid](const std::string &response) {
                 try {
                     auto transferResponse = folly::parseJson(response);
                     LOG_DBG(1) << "Scheduled transfer with ID: "
                                << transferResponse["transferId"];
                 }
                 catch (...) {
                     LOG(ERROR)
                         << "Invalid transfer request response: " << response;
                     throw;
                 }
             })
                .onError([uuid = uuid](std::exception const &e) {
                    LOG(ERROR) << "Transfer request for file " << uuid
                               << " failed due to: " << e.what();
                });
        }
        else {
            prefetchRange =
                boost::icl::discrete_interval<off_t>::right_open(0, fileSize);

            worthPrefetching = true;
            fullFilePrefetchRequested = true;
        }

        fuseFileHandle->setFullPrefetchTriggered();

        LOG_DBG(2) << "Requesting full file prefetch for " << uuid
                   << " in range " << prefetchRange;

        prefetchType = IOTraceLogger::PrefetchType::FULL;
    }
    // Check if we should consider block cluster prefetch
    else if (m_randomReadPrefetchClusterWindow) {
        // Calculate the current clustering window size based on initial
        // window size, grow factor and current replication progress
        const auto windowSize = m_randomReadPrefetchClusterWindow *
            (1.0 +
                m_randomReadPrefetchClusterWindowGrowFactor * fileSize *
                    replicationProgress / m_randomReadPrefetchClusterWindow);

        // Get the number of different blocks in the current clustering window
        // around the current read offset - if higher than threshold, request
        // synchronous prefetch of that window clipped to file size
        auto leftRange = std::max<off_t>(0, offset - windowSize / 2);
        auto rightRange = std::min<off_t>(offset + windowSize / 2, fileSize);
        auto blocksInRange = fileLocation->blocksInRange(leftRange, rightRange);

        auto prefetchBlockThreshold = m_randomReadPrefetchClusterBlockThreshold;
        if (m_clusterPrefetchThresholdRandom) {
            prefetchBlockThreshold =
                m_clusterPrefetchDistribution(m_clusterPrefetchRandomGenerator);
        }

        if (blocksInRange > prefetchBlockThreshold) {
            LOG_DBG(1) << "Requesting clustered prefetch of block ["
                       << leftRange << ", " << rightRange << ") for file "
                       << uuid << ". " << blocksInRange
                       << " blocks in range (prefetch threshold: "
                       << prefetchBlockThreshold << ")";

            prefetchRange = boost::icl::discrete_interval<off_t>::right_open(
                leftRange, rightRange);

            prefetchType = IOTraceLogger::PrefetchType::CLUSTER;
            prefetchPriority = SYNCHRONIZE_BLOCK_PRIORITY_CLUSTER_PREFETCH;

            worthPrefetching = true;
            clusterPrefetchRequested = true;
        }
    }

    if (!fullFilePrefetchRequested && !clusterPrefetchRequested) {
        prefetchRange = boost::icl::left_subtract(
            wantToPrefetchRange & possibleRange, availableRange);

        if (boost::icl::size(prefetchRange) > 0) {
            worthPrefetching = boost::icl::size(prefetchRange &
                                   fuseFileHandle->lastPrefetch()) == 0 ||
                boost::icl::size(boost::icl::left_subtract(
                    prefetchRange, fuseFileHandle->lastPrefetch())) >=
                    boost::icl::size(prefetchRange) / 2;

            if (worthPrefetching) {
                fuseFileHandle->setLastPrefetch(prefetchRange);
                LOG_DBG(1) << "Requesting linear prefetch for file " << uuid
                           << " in range " << prefetchRange;

                prefetchType = IOTraceLogger::PrefetchType::LINEAR;
                prefetchPriority = SYNCHRONIZE_BLOCK_PRIORITY_LINEAR_PREFETCH;
            }
        }
    }

    if (boost::icl::size(prefetchRange) > 0 && worthPrefetching) {
        prefetchSize = boost::icl::size(prefetchRange);
        // Request the calculated prefetch block asynchronously
        m_context->communicator()
            ->communicate<messages::fuse::FileLocationChanged>(
                messages::fuse::SynchronizeBlock{
                    uuid.toStdString(), prefetchRange, prefetchPriority, false})
            .then([this](messages::fuse::FileLocationChanged locationUpdate) {
                m_runInFiber(
                    [ this, locationUpdate = std::move(locationUpdate) ] {
                        if (locationUpdate.changeStartOffset() &&
                            locationUpdate.changeEndOffset())
                            m_metadataCache.updateLocation(
                                *locationUpdate.changeStartOffset(),
                                *locationUpdate.changeEndOffset(),
                                locationUpdate.fileLocation());
                        else
                            m_metadataCache.updateLocation(
                                locationUpdate.fileLocation());
                    });
            });
    }

    return {prefetchSize, prefetchType};
}

std::size_t FsLogic::write(const folly::fbstring &uuid,
    const std::uint64_t fuseFileHandleId, const off_t offset,
    folly::IOBufQueue buf, const int retriesLeft,
    std::unique_ptr<IOTraceWrite> ioTraceEntry)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(fuseFileHandleId)
                << LOG_FARG(offset) << LOG_FARG(buf.chainLength());

    if (buf.empty()) {
        LOG_DBG(2) << "Write called with empty buffer - skipping";
        return 0;
    }

    if (m_ioTraceLoggerEnabled && !ioTraceEntry) {
        ioTraceEntry = std::make_unique<IOTraceWrite>();
        ioTraceEntry->opType = IOTraceLogger::OpType::WRITE;
        ioTraceEntry->uuid = uuid;
        ioTraceEntry->handleId = fuseFileHandleId;
        ioTraceEntry->retries = 0;
        std::get<0>(ioTraceEntry->arguments) = offset;
        std::get<1>(ioTraceEntry->arguments) = 0;
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
        if (e.code().value() == EAGAIN && retriesLeft) {
            fiberRetryDelay(retriesLeft);
            return write(uuid, fuseFileHandleId, offset, std::move(buf),
                retriesLeft - 1, std::move(ioTraceEntry));
        }
        else if (e.code().value() != EPERM && e.code().value() != EACCES) {
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
                   << " to force proxy cache after direct write failed";

        m_forceProxyIOCache.add(uuid);

        LOG_DBG(1) << "Writing requested block for " << uuid
                   << " via proxy fallback";

        return write(uuid, fuseFileHandleId, offset, std::move(buf),
            retriesLeft, std::move(ioTraceEntry));
    }

    m_eventManager.emit<events::FileWritten>(uuid.toStdString(), offset,
        bytesWritten, fileBlock.storageId(), fileBlock.fileId());

    auto writtenRange = boost::icl::discrete_interval<off_t>::right_open(
        offset, offset + bytesWritten);

    LOG_DBG(2) << "Written " << bytesWritten << " bytes to file " << uuid
               << " at offset " << offset << " on storage "
               << fileBlock.storageId();

    m_metadataCache.addBlock(uuid, writtenRange, std::move(fileBlock));

    if (m_ioTraceLoggerEnabled) {
        std::get<1>(ioTraceEntry->arguments) = bytesWritten;
        using namespace std::chrono;
        ioTraceEntry->duration = duration_cast<microseconds>(
            system_clock::now() - ioTraceEntry->timestamp);
        m_ioTraceLogger->log(*ioTraceEntry);
    }

    return bytesWritten;
}

FileAttrPtr FsLogic::mkdir(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const mode_t mode)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name) << LOG_FARG(mode);

    IOTRACE_START()

    // TODO: CreateDir should probably also return attrs
    communicate(messages::fuse::CreateDir{parentUuid.toStdString(),
                    name.toStdString(), mode},
        m_providerTimeout);

    LOG_DBG(2) << "Created directory " << name << " in " << parentUuid;

    // TODO: Provider returns uuid of the created dir, no need for lookup
    auto attr = m_metadataCache.getAttr(parentUuid, name);

    IOTRACE_END(IOTraceMkdir, IOTraceLogger::OpType::MKDIR, parentUuid, 0, name,
        attr->uuid(), mode)

    return attr;
}

FileAttrPtr FsLogic::mknod(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const mode_t mode)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name) << LOG_FARG(mode);

    IOTRACE_START()

    messages::fuse::MakeFile msg{parentUuid, name, mode};
    auto attr = communicate<FileAttr>(std::move(msg), m_providerTimeout);
    auto sharedAttr = std::make_shared<FileAttr>(std::move(attr));
    m_metadataCache.putAttr(sharedAttr);

    m_readdirCache->invalidate(parentUuid);

    LOG_DBG(2) << "Created node " << name << " in " << parentUuid
               << " with uuid " << attr.uuid();

    IOTRACE_END(IOTraceMknod, IOTraceLogger::OpType::MKNOD, parentUuid, 0, name,
        sharedAttr->uuid(), mode)

    return sharedAttr;
}

std::pair<FileAttrPtr, std::uint64_t> FsLogic::create(
    const folly::fbstring &parentUuid, const folly::fbstring &name,
    const mode_t mode, const int flags)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name) << LOG_FARG(mode)
                << LOG_FARG(flags);

    IOTRACE_START()

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

    LOG_DBG(2) << "Created file " << name << " in " << parentUuid
               << " with uuid " << uuid;

    m_readdirCache->invalidate(parentUuid);

    IOTRACE_END(IOTraceCreate, IOTraceLogger::OpType::CREATE, parentUuid,
        fuseFileHandleId, name, sharedAttr->uuid(), mode, flags)

    return {sharedAttr, fuseFileHandleId};
}

void FsLogic::unlink(
    const folly::fbstring &parentUuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name);

    IOTRACE_GUARD(
        IOTraceUnlink, IOTraceLogger::OpType::UNLINK, parentUuid, 0, name)

    // TODO: directly order provider to delete {parentUuid, name}
    auto attr = m_metadataCache.getAttr(parentUuid, name);
    communicate(messages::fuse::DeleteFile{attr->uuid().toStdString()},
        m_providerTimeout);

    m_metadataCache.markDeleted(attr->uuid());

    m_readdirCache->invalidate(parentUuid);

    LOG_DBG(2) << "Deleted file " << name << " in " << parentUuid
               << " with uuid " << attr->uuid();
}

void FsLogic::rename(const folly::fbstring &parentUuid,
    const folly::fbstring &name, const folly::fbstring &newParentUuid,
    const folly::fbstring &newName)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name)
                << LOG_FARG(newParentUuid) << LOG_FARG(newName);

    IOTRACE_START()

    // TODO: directly order provider to rename {parentUuid, name}
    auto attr = m_metadataCache.getAttr(parentUuid, name);
    auto oldUuid = attr->uuid();

    auto renamed = communicate<messages::fuse::FileRenamed>(
        messages::fuse::Rename{oldUuid.toStdString(),
            newParentUuid.toStdString(), newName.toStdString()},
        m_providerTimeout);

    m_metadataCache.rename(oldUuid, newParentUuid, newName, renamed.newUuid());

    LOG_DBG(2) << "Renamed file " << name << " in " << parentUuid << " to "
               << newName << " in " << newParentUuid;

    for (auto &child : renamed.childEntries())
        m_metadataCache.rename(child.oldUuid(), child.newParentUuid(),
            child.newName(), child.newUuid());

    IOTRACE_END(IOTraceRename, IOTraceLogger::OpType::RENAME, parentUuid, 0,
        name, newParentUuid, newName, renamed.newUuid())
}

FileAttrPtr FsLogic::setattr(
    const folly::fbstring &uuid, const struct stat &attr, const int toSet)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(toSet);

    IOTRACE_GUARD(IOTraceSetAttr, IOTraceLogger::OpType::SETATTR, uuid, 0,
        toSet, attr.st_mode, attr.st_size, attr.st_atime, attr.st_mtime)

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

        LOG_DBG(2) << "Changed mode of " << uuid << " to "
                   << LOG_OCT(normalizedMode);
    }

    if (toSet & FUSE_SET_ATTR_SIZE) {
        communicate(messages::fuse::Truncate{uuid.toStdString(), attr.st_size},
            m_providerTimeout);
        m_metadataCache.truncate(uuid, attr.st_size);
        m_eventManager.emit<events::FileTruncated>(
            uuid.toStdString(), attr.st_size);

        LOG_DBG(2) << "Truncated file " << uuid << " to size " << attr.st_size
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
        LOG_DBG(2) << "Changed atime of " << uuid << " to " << attr.st_atime;
    }
    if (toSet & FUSE_SET_ATTR_MTIME) {
        updateTimes.mtime(
            std::chrono::system_clock::from_time_t(attr.st_mtime));
        LOG_DBG(2) << "Changed mtime of " << uuid << " to " << attr.st_atime;
    }
#if defined(FUSE_SET_ATTR_ATIME_NOW)
    if (toSet & FUSE_SET_ATTR_ATIME_NOW) {
        updateTimes.atime(now);
        LOG_DBG(2) << "Changed atime of " << uuid << " to now";
    }
#endif
#if defined(FUSE_SET_ATTR_MTIME_NOW)
    if (toSet & FUSE_SET_ATTR_MTIME_NOW) {
        updateTimes.mtime(now);
        LOG_DBG(2) << "Changed mtime of " << uuid << " to now";
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

    IOTRACE_GUARD(
        IOTraceGetXAttr, IOTraceLogger::OpType::GETXATTR, uuid, 0, name)

    folly::fbstring result;

    if (name == ONE_XATTR("uuid")) {
        return "\"" + uuid + "\"";
    }
    else if (name == ONE_XATTR("file_id")) {
        return "\"" + m_metadataCache.getDefaultBlock(uuid).fileId() + "\"";
    }
    else if (name == ONE_XATTR("storage_id")) {
        return "\"" + m_metadataCache.getDefaultBlock(uuid).storageId() + "\"";
    }
    else if (name == ONE_XATTR("space_id")) {
        return "\"" + m_metadataCache.getSpaceId(uuid) + "\"";
    }
    else if (name == ONE_XATTR("access_type")) {
        auto accessType = m_helpersCache->getAccessType(
            m_metadataCache.getDefaultBlock(uuid).storageId());
        if (accessType == cache::HelpersCache::AccessType::DIRECT)
            return "\"direct\"";
        else if (accessType == cache::HelpersCache::AccessType::PROXY)
            return "\"proxy\"";
        else
            return "\"unknown\"";
    }
    else if (name == ONE_XATTR("file_blocks_count")) {
        auto forceLocationUpdate =
            !m_fsSubscriptions.isSubscribedToFileLocationChanged(uuid);
        return "\"" +
            std::to_string(
                m_metadataCache.getLocation(uuid, forceLocationUpdate)
                    ->blocksCount()) +
            "\"";
    }
    else if (name == ONE_XATTR("file_blocks")) {
        std::size_t size = m_metadataCache.getAttr(uuid)->size().value_or(0);
        if (size == 0)
            return "\"empty\"";
        else {
            auto forceLocationUpdate =
                !m_fsSubscriptions.isSubscribedToFileLocationChanged(uuid);
            return "\"[" +
                m_metadataCache.getLocation(uuid, forceLocationUpdate)
                    ->progressString(size, XATTR_FILE_BLOCKS_MAP_LENGTH) +
                "]\"";
        }
    }
    else if (name == ONE_XATTR("replication_progress")) {
        std::size_t size = m_metadataCache.getAttr(uuid)->size().value_or(0);

        auto forceLocationUpdate =
            !m_fsSubscriptions.isSubscribedToFileLocationChanged(uuid);
        auto replicationProgress =
            m_metadataCache.getLocation(uuid, forceLocationUpdate)
                ->replicationProgress(size);

        return "\"" +
            std::to_string((int)std::floor(replicationProgress * 100)) + "%\"";
    }

    messages::fuse::GetXAttr getXAttrRequest{uuid, name};
    auto xattr =
        communicate<messages::fuse::XAttr>(getXAttrRequest, m_providerTimeout);
    result = xattr.value();

    LOG_DBG(2) << "Received xattr " << name << " value for file " << uuid;

    return result;
}

void FsLogic::setxattr(const folly::fbstring &uuid, const folly::fbstring &name,
    const folly::fbstring &value, bool create, bool replace)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(name) << LOG_FARG(value)
                << LOG_FARG(create) << LOG_FARG(replace);

    IOTRACE_GUARD(IOTraceSetXAttr, IOTraceLogger::OpType::SETXATTR, uuid, 0,
        name, value, create, replace)

    messages::fuse::SetXAttr setXAttrRequest{
        uuid, name, value, create, replace};
    communicate<messages::fuse::FuseResponse>(
        setXAttrRequest, m_providerTimeout);

    LOG_DBG(2) << "Set xattr " << name << " value for file " << uuid;
}

void FsLogic::removexattr(
    const folly::fbstring &uuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(name);

    IOTRACE_GUARD(
        IOTraceRemoveXAttr, IOTraceLogger::OpType::REMOVEXATTR, uuid, 0, name)

    messages::fuse::RemoveXAttr removeXAttrRequest{uuid, name};
    communicate<messages::fuse::FuseResponse>(
        removeXAttrRequest, m_providerTimeout);

    LOG_DBG(2) << "Removed xattr " << name << " from file " << uuid;
}

folly::fbvector<folly::fbstring> FsLogic::listxattr(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    IOTRACE_GUARD(IOTraceListXAttr, IOTraceLogger::OpType::LISTXATTR, uuid, 0)

    using namespace one::messages::fuse;

    folly::fbvector<folly::fbstring> result;

    ListXAttr listXAttrRequest{uuid};
    XAttrList fuseResponse =
        communicate<XAttrList>(listXAttrRequest, m_providerTimeout);

    for (const auto &xattrName : fuseResponse.xattrNames()) {
        result.push_back(xattrName.c_str());
    }

    result.push_back(ONE_XATTR("uuid"));
    if (m_metadataCache.getAttr(uuid)->type() == FileAttr::FileType::regular) {
        result.push_back(ONE_XATTR("space_id"));
        result.push_back(ONE_XATTR("file_id"));
        result.push_back(ONE_XATTR("storage_id"));
        result.push_back(ONE_XATTR("access_type"));
        result.push_back(ONE_XATTR("file_blocks"));
        result.push_back(ONE_XATTR("file_blocks_count"));
        result.push_back(ONE_XATTR("replication_progress"));
    }

    LOG_DBG(2) << "Received xattr list for file " << uuid;

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
        uuid.toStdString(), range, SYNCHRONIZE_BLOCK_PRIORITY_IMMEDIATE};

    auto syncResponse = communicate<messages::fuse::SyncResponse>(
        std::move(request), m_providerTimeout);

    m_metadataCache.updateLocation(syncResponse.fileLocation());

    return syncResponse.checksum();
}

void FsLogic::sync(const folly::fbstring &uuid,
    const boost::icl::discrete_interval<off_t> &range)
{
    messages::fuse::SynchronizeBlock request{
        uuid.toStdString(), range, SYNCHRONIZE_BLOCK_PRIORITY_IMMEDIATE, false};
    auto fileLocationUpdate = communicate<messages::fuse::FileLocationChanged>(
        std::move(request), m_providerTimeout);

    if (fileLocationUpdate.changeStartOffset() &&
        fileLocationUpdate.changeEndOffset())
        m_metadataCache.updateLocation(
            *(fileLocationUpdate.changeStartOffset()),
            *(fileLocationUpdate.changeEndOffset()),
            fileLocationUpdate.fileLocation());
    else
        m_metadataCache.updateLocation(fileLocationUpdate.fileLocation());
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

void FsLogic::fiberRetryDelay(int retriesLeft)
{
    auto delayRange = FSLOGIC_RETRY_DELAYS[FSLOGIC_RETRY_COUNT - retriesLeft];
    auto delay = std::chrono::milliseconds(delayRange.first +
        (std::rand() % (delayRange.second - delayRange.first + 1)));

    LOG_DBG(1) << "Retrying FsLogic operation due to resource "
                  "temporarily unavailable error in "
               << delay.count() << "ms. Retries left: " << retriesLeft;

    folly::fibers::Baton baton;
    baton.timed_wait(delay);
}

std::shared_ptr<IOTraceLogger> FsLogic::createIOTraceLogger()
{
    auto now = std::chrono::system_clock::now();
    auto nowTimeT = std::chrono::system_clock::to_time_t(now);
    char nowBuf[512];

    std::tm nowTm = *std::localtime(&nowTimeT);
    std::strftime(nowBuf, 512, "%Y%m%dT%H%M%S", &nowTm);
    auto traceFilePath = m_context->options()->getLogDirPath() /
        (std::string{"iotrace-"} + nowBuf + ".csv");

    return IOTraceLogger::make(traceFilePath.native());
}

} // namespace fslogic
} // namespace client
} // namespace one
