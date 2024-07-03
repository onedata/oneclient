/**
 * @file s3LogicFile.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Logic.h"

#include "futureUtils.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileCreated.h"
#include "messages/fuse/fileList.h"
#include "messages/fuse/fileLocation.h"
#include "messages/fuse/fileLocationChanged.h"
#include "messages/fuse/fsync.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/multipartParts.h"
#include "messages/fuse/multipartUpload.h"
#include "messages/fuse/release.h"
#include "messages/fuse/reportFileWritten.h"
#include "messages/fuse/synchronizeBlock.h"
#include "monitoring/monitoring.h"

#include <spdlog/spdlog.h>

#include <tuple>
#include <utility>

namespace one {
namespace s3 {

using one::client::fslogic::FuseFileHandle;
using one::messages::fuse::FileAttr;
using one::messages::fuse::FileBlock;
using one::messages::fuse::FileLocation;
using one::messages::fuse::FileLocationChanged;
using one::messages::fuse::FSync;
using one::messages::fuse::FuseResponse;
using one::messages::fuse::GetFileLocation;
using one::messages::fuse::Release;
using one::messages::fuse::ReportFileWritten;
using one::messages::fuse::SynchronizeBlock;

folly::Future<folly::Unit> S3Logic::close(
    const folly::fbstring &uuid, const std::string &requestId)
{
    std::lock_guard<std::mutex> lockGuard{m_handleMutex};
    if (m_requestHandles.find(requestId) == m_requestHandles.end()) {
        LOG_DBG(1) << "Trying to close invalid handle for request "
                   << requestId;
        return {};
    }

    LOG_DBG(3) << "Trying to close file for request " << requestId;

    auto fileHandle = m_requestHandles.at(requestId);

    folly::fbvector<folly::Future<folly::Unit>> releaseFutures;
    for (auto &helperHandle : fileHandle->helperHandles())
        releaseFutures.emplace_back(helperHandle->release());

    return folly::collectAll(releaseFutures)
        .via(m_executor.get())
        .thenTry([this,
                     fileHandleId =
                         fileHandle->providerHandleId()->toStdString(),
                     uuid, requestId](auto && /*unit*/) {
            return communicate(FSync{uuid.toStdString(), false, fileHandleId});
        })
        .thenTry(
            [this, fileHandleId = fileHandle->providerHandleId()->toStdString(),
                uuid, requestId](auto && /*unit*/) {
                LOG_DBG(3) << "Closing file for request " << requestId;
                return communicate(Release{uuid.toStdString(), fileHandleId});
            })
        .thenTry([this, requestId](auto && /*unit*/) {
            std::lock_guard<std::mutex> lockGuard{m_handleMutex};
            if (m_requestHandles.find(requestId) != m_requestHandles.end()) {
                m_requestFileHandleFlags.erase(requestId);
                m_requestHandles.erase(requestId);
                m_requestContext.erase(requestId);
            }
        });
}

folly::Future<std::size_t> S3Logic::write(
    std::shared_ptr<FuseFileHandle> fileHandle, folly::fbstring uuid,
    const std::string &requestId, std::shared_ptr<folly::IOBuf> buf,
    const size_t baseOffset)
{
    if (buf->empty()) {
        LOG_DBG(2) << "Write called with empty buffer - skipping";
        return 0;
    }

    auto &context = getRequestContext(requestId);
    const auto &attr = context.attr;
    const auto &spaceId = context.spaceId;
    const auto &location = context.location;
    const size_t offset = baseOffset + context.offset;
    auto fileBlock = FileBlock{location.storageId(), location.fileId()};

    folly::IOBufQueue bufq{folly::IOBufQueue::cacheChainLength()};
    bufq.append(buf->clone());

    LOG_DBG(3) << "Writing to helper with timeout [ms]: "
               << std::chrono::duration_cast<std::chrono::milliseconds>(
                      m_providerTimeout)
                      .count();

    LOG_DBG(3) << "Writing " << bufq.chainLength() << " bytes at offset "
               << offset;

    using namespace std::chrono_literals;
    return fileHandle
        ->getHelperHandle(
            attr.uuid(), spaceId, location.storageId(), location.fileId())
        .thenTry([this, bufq = std::move(bufq), uuid = std::move(uuid), offset](
                     auto &&maybeHelperHandle) mutable {
            return maybeHelperHandle.value()
                ->write(offset, std::move(bufq), {})
                .via(m_executor.get())
                .thenTry(
                    [this, uuid = std::move(uuid), offset](auto &&written) {
                        if (written.hasException()) {
                            LOG(ERROR) << "Write failed: "
                                       << written.exception().what();
                            written.throwUnlessValue();
                        }

                        return communicate(ReportFileWritten{uuid.toStdString(),
                                               offset, written.value()})
                            .via(m_executor.get())
                            .thenTry([size = written.value()](
                                         auto && /*unit*/) { return size; });
                    });
        });
}

folly::Future<folly::IOBufQueue> S3Logic::read(
    std::shared_ptr<FuseFileHandle> fileHandle, const folly::fbstring &spaceId,
    const FileAttr &attr, const std::size_t offset, const std::size_t size)
{
    return communicate<FileLocation>(GetFileLocation{attr.uuid().toStdString()})
        .thenTry([this, attr, offset, size, spaceId, fileHandle](
                     folly::Try<FileLocation> &&maybeLocation)
                     -> folly::Future<folly::IOBufQueue> {
            const auto fileSize = *attr.size();
            const auto &uuid = attr.uuid();
            const auto possibleRange =
                boost::icl::discrete_interval<off_t>::right_open(0, fileSize);
            FileLocation location;

            if (maybeLocation.hasValue())
                location = std::move(maybeLocation).value();

            const auto requestedRange =
                boost::icl::discrete_interval<off_t>::right_open(
                    offset, offset + size);

            auto neededRange = requestedRange & possibleRange;

            assert(boost::icl::size(neededRange) > 0);

            if (boost::icl::size(neededRange) <= 0) {
                return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
            }

            auto replicatedBlocksRange = location.blocks() & neededRange;

            // TODO: Refactor this block to async thenTry
            if (replicatedBlocksRange.size() < size) {
                // request prefetch and wait
                auto syncRange =
                    boost::icl::discrete_interval<off_t>::right_open(offset,
                        offset +
                            std::max<std::size_t>(
                                size, m_minPrefetchBlockSize));

                SynchronizeBlock request{uuid.toStdString(), syncRange,
                    SYNCHRONIZE_BLOCK_PRIORITY_IMMEDIATE, false};
                auto fileLocationUpdate =
                    communicate<FileLocationChanged>(std::move(request));
                location = fileLocationUpdate.value().fileLocation();
            }

            auto availableBlockIt = location.blocks().find(
                boost::icl::discrete_interval<off_t>(offset));

            if (availableBlockIt == location.blocks().end())
                return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};

            boost::icl::discrete_interval<off_t> availableRange{
                availableBlockIt->first};
            FileBlock fileBlock{availableBlockIt->second};

            const auto neededAvailableRange = availableRange & neededRange;

            if (boost::icl::size(neededAvailableRange) <= 0) {
                throw one::helpers::makePosixException(ERANGE);
            }

            const std::size_t availableSize =
                boost::icl::size(neededAvailableRange);
            const std::size_t continuousSize =
                boost::icl::size(boost::icl::left_subtract(availableRange,
                    boost::icl::discrete_interval<off_t>::right_open(
                        0, offset)));

            return fileHandle
                ->getHelperHandle(attr.uuid(), spaceId, fileBlock.storageId(),
                    fileBlock.fileId())
                .thenTry([offset, availableSize, continuousSize](
                             auto &&maybeHelperHandle) {
                    return maybeHelperHandle.value()->readContinuous(
                        offset, availableSize, continuousSize);
                });
        });
}

folly::Future<folly::IOBufQueue> S3Logic::read(
    std::shared_ptr<one::client::fslogic::FuseFileHandle> fileHandle,
    const std::string &requestId, const std::size_t size,
    const std::size_t requestOffset, const std::size_t requestSize,
    const folly::Optional<folly::fbstring> & /*checksum*/)
{
    auto &context = getRequestContext(requestId);
    const auto &attr = context.attr;
    const auto &spaceId = context.spaceId;
    const auto &location = context.location;
    const size_t offset = context.offset;

    // Calculate file block from file location
    assert(attr.size().hasValue());
    const size_t fileSize = attr.size().value();

    const auto possibleRange =
        boost::icl::discrete_interval<off_t>::right_open(0, fileSize);

    const auto requestedRange =
        boost::icl::discrete_interval<off_t>::right_open(
            offset + requestOffset, offset + requestOffset + size);

    const auto totalRequestRange =
        boost::icl::discrete_interval<off_t>::right_open(
            requestOffset, requestOffset + requestSize);

    auto wantedRange = requestedRange & possibleRange & totalRequestRange;

    if (boost::icl::size(wantedRange) <= 0) {
        return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
    }

    boost::icl::discrete_interval<off_t> availableRange = possibleRange;

    auto fileBlock = FileBlock{location.storageId(), location.fileId()};

    const auto wantedAvailableRange = availableRange & wantedRange;

    if (boost::icl::size(wantedAvailableRange) <= 0) {
        throw one::helpers::makePosixException(ERANGE);
    }

    const std::size_t wantedAvailableSize =
        boost::icl::size(wantedAvailableRange);
    const std::size_t continuousSize = wantedAvailableSize;

    assert(offset + requestOffset + wantedAvailableSize <= fileSize);
    assert(wantedAvailableSize <= size);

    return fileHandle
        ->getHelperHandle(
            attr.uuid(), spaceId, fileBlock.storageId(), fileBlock.fileId())
        .thenTry([=](auto &&maybeHelperHandle) {
            return maybeHelperHandle.value()
                ->readContinuous(
                    offset + requestOffset, wantedAvailableSize, continuousSize)
                .thenValue([this, requestId, size, offset, requestOffset,
                               requestSize](folly::IOBufQueue &&buf) {
                    LOG_DBG(3)
                        << "[" << requestId << ", " << offset + requestOffset
                        << ", " << requestSize << "] Read from helper "
                        << buf.chainLength() << "/" << size << " at offset "
                        << offset + requestOffset;

                    getRequestContext(requestId).offset += buf.chainLength();
                    return std::move(buf);
                });
        });
}

} // namespace s3
} // namespace one