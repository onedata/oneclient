/**
 * @file s3Logic.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Logic.h"

#include "futureUtils.h"
#include "messages/fuse/createFile.h"
#include "messages/fuse/createPath.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileCreated.h"
#include "messages/fuse/fileList.h"
#include "messages/fuse/fileLocation.h"
#include "messages/fuse/fileLocationChanged.h"
#include "messages/fuse/fileOpened.h"
#include "messages/fuse/getChildAttr.h"
#include "messages/fuse/getFileAttrByPath.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/multipartUpload.h"
#include "messages/fuse/openFile.h"
#include "messages/fuse/release.h"
#include "messages/fuse/resolveGuid.h"
#include "messages/fuse/synchronizeBlock.h"
#include "monitoring/monitoring.h"

#include <spdlog/spdlog.h>

#include <tuple>
#include <utility>

namespace one {
namespace s3 {

using one::client::fslogic::FuseFileHandle;
using one::messages::fuse::CreateFile;
using one::messages::fuse::CreatePath;
using one::messages::fuse::FileAttr;
using one::messages::fuse::FileChildrenAttrs;
using one::messages::fuse::FileCreated;
using one::messages::fuse::FileLocation;
using one::messages::fuse::FileLocationChanged;
using one::messages::fuse::FileOpened;
using one::messages::fuse::FuseResponse;
using one::messages::fuse::GetChildAttr;
using one::messages::fuse::GetFileAttrByPath;
using one::messages::fuse::GetFileLocation;
using one::messages::fuse::MultipartUpload;
using one::messages::fuse::OpenFile;
using one::messages::fuse::Release;
using one::messages::fuse::ResolveGuid;
using one::messages::fuse::SynchronizeBlock;
using one::messages::fuse::Uuid;

folly::fbstring getFileNameFromPath(const folly::fbstring &path)
{
    std::vector<std::string> pathTokens;
    folly::split("/", path, pathTokens, false);
    return pathTokens.back();
}

folly::fbstring getMultipartUploadTemporaryFileName(
    const folly::fbstring &key, const folly::fbstring &uploadId)
{
    return fmt::format("{}/{}/{}", ONEDATA_S3_MULTIPART_PREFIX, uploadId,
        getFileNameFromPath(key).toStdString());
}

folly::fbstring getCompleteUploadTemporaryFileName(const std::string &requestId)
{
    return fmt::format("{}-tmp", requestId);
}

folly::fbstring getMultipartUploadTemporaryDir(const folly::fbstring &uploadId)
{
    return fmt::format("{}/{}", ONEDATA_S3_MULTIPART_PREFIX, uploadId);
}

S3Logic::S3Logic(std::shared_ptr<one::client::options::Options> options,
    folly::fbstring token,
    std::shared_ptr<folly::IOThreadPoolExecutor> executor)
    : m_providerTimeout{options->getProviderTimeout()}
    , m_options{std::move(options)}
    , m_connected{false}
    , m_token{std::move(token)}
    , m_minPrefetchBlockSize{m_options->getMinimumBlockPrefetchSize()}
    , m_executor{std::move(executor)}
{
    m_context = std::make_shared<OneS3Context>();
    m_context->setScheduler(
        std::make_shared<Scheduler>(m_options->getSchedulerThreadCount()));
    m_context->setOptions(m_options);
}

folly::Future<std::shared_ptr<S3Logic>> S3Logic::connect()
{
    using messages::handshake::ClientType;
    using one::client::cache::HelpersCache;

    if (m_connected)
        return shared_from_this();

    m_authManager =
        one::client::getTokenAuthManager<OneS3Context>(m_context, m_token);
    auto sessionId = one::client::generateSessionId();
    m_configuration = one::client::getConfiguration<OneS3Context>(
        sessionId, m_authManager, m_context, ClientType::ones3);

    auto communicator = getCommunicator(sessionId, m_authManager, m_context,
        messages::handshake::ClientType::ones3);
    m_context->setCommunicator(communicator);
    communicator->connect();
    m_authManager->scheduleRefresh(
        one::client::auth::RESTRICTED_MACAROON_REFRESH);

    if (m_context->options()->isMessageTraceLoggerEnabled()) {
        using namespace std::chrono;
        auto messageLogPath = m_context->options()->getLogDirPath() /
            fmt::format("message-log-{}.txt",
                duration_cast<seconds>(system_clock::now().time_since_epoch())
                    .count());
        m_context->communicator()->enableMessageLog(
            "message_trace_log", messageLogPath.string());
    }

    m_helpersCache.setCache(std::make_unique<HelpersCache<OneS3Communicator>>(
        *communicator, m_context->scheduler(), *m_context->options()));

    m_rootUuid = m_configuration->rootUuid();

    m_connected = true;

    return folly::makeSemiFuture(shared_from_this()).via(m_executor.get());
}

S3RequestContext &S3Logic::getRequestContext(const std::string &requestId)
{
    std::lock_guard<std::mutex> lockGuard{m_handleMutex};
    return m_requestContext.at(requestId);
}

folly::Future<FileLocation> S3Logic::ensureFileLocationForRange(
    const FileAttr &attr, const std::size_t offset, const std::size_t size)
{
    return communicate<FileLocation>(GetFileLocation{attr.uuid().toStdString()})
        .via(m_executor.get())
        .thenTry([this, attr, offset, size](
                     folly::Try<FileLocation> &&maybeLocation)
                     -> folly::Future<FileLocation> {
            FileLocation location;

            if (maybeLocation.hasValue())
                location = std::move(maybeLocation).value();

            if (size == 0) {
                // The request is an upload - we don't need to prefetch
                // data
                return std::move(location);
            }

            const auto fileSize = *attr.size();
            const auto &uuid = attr.uuid();
            const auto possibleRange =
                boost::icl::discrete_interval<off_t>::right_open(0, fileSize);

            const auto requestedRange =
                boost::icl::discrete_interval<off_t>::right_open(
                    offset, offset + size);

            auto neededRange = requestedRange & possibleRange;

            assert(boost::icl::size(neededRange) > 0);

            if (boost::icl::size(neededRange) <= 0) {
                throw one::helpers::makePosixException(ERANGE);
            }

            auto replicatedBlocksRange = location.blocks() & neededRange;

            if (replicatedBlocksRange.size() < size) {
                // request block synchronization and wait
                auto syncRange =
                    boost::icl::discrete_interval<off_t>::right_open(offset,
                        offset +
                            std::max<std::size_t>(
                                size, m_minPrefetchBlockSize));

                SynchronizeBlock request{uuid.toStdString(), syncRange,
                    SYNCHRONIZE_BLOCK_PRIORITY_IMMEDIATE, false};

                return communicate<FileLocationChanged>(std::move(request))
                    .thenTry([](auto &&maybeFileLocationChanged) {
                        if (maybeFileLocationChanged.hasException())
                            throw one::helpers::makePosixException(ERANGE);

                        return maybeFileLocationChanged.value().fileLocation();
                    });
            }

            return std::move(location);
        });
}

folly::Future<Uuid> S3Logic::resolveGuid(const folly::fbstring &path)
{
    return communicate<Uuid>(ResolveGuid{path.toStdString()});
}

folly::Future<FileAttr> S3Logic::getFileAttr(
    const folly::fbstring &spaceId, const folly::fbstring &path)
{
    std::vector<std::string> pathVector;
    folly::split("/", path, pathVector, true);

    std::reverse(pathVector.begin(), pathVector.end());

    return getFileAttr(spaceId, pathVector);
}

folly::Future<FileAttr> S3Logic::getBucketAttr(const folly::fbstring &bucket)
{
    if (m_bucketIdCache.find(bucket) != m_bucketIdCache.end())
        return folly::makeFuture(m_bucketIdCache.at(bucket));

    return communicate<FileAttr>(GetChildAttr{m_rootUuid, bucket})
        .via(m_executor.get())
        .thenTry([this, bucket](folly::Try<FileAttr> &&bucketAttr) {
            m_bucketIdCache.emplace(bucket, bucketAttr.value());
            return std::move(bucketAttr);
        });
}

folly::Future<FileAttr> S3Logic::getBucketTmpDirAttr(
    const folly::fbstring &bucket)
{
    if (m_bucketTmpDirCache.find(bucket) != m_bucketTmpDirCache.end())
        return folly::makeFuture(m_bucketTmpDirCache.at(bucket));

    return getBucketAttr(bucket).thenValue([this, bucket](auto &&bucketAttr) {
        const auto tmpDirId =
            one::client::util::uuid::uuidToTmpDirId(bucketAttr.uuid());

        return communicate<FileAttr>(
            CreatePath{tmpDirId, ONEDATA_S3_MULTIPART_PREFIX})
            .thenTry([this, bucket](folly::Try<FileAttr> &&tmpDirAttr) {
                m_bucketTmpDirCache.emplace(bucket, tmpDirAttr.value());
                return std::move(tmpDirAttr);
            });
    });
}

folly::Future<FileAttr> S3Logic::getFileAttr(
    const folly::fbstring &parentId, std::vector<std::string> path)
{
    assert(!path.empty());

    if (path.size() == 1) {
        return communicate<FileAttr>(GetChildAttr{parentId, path[0]});
    }

    auto nextChildName = path.back();
    path.pop_back();

    return communicate<FileAttr>(GetChildAttr{parentId, nextChildName})
        .thenValue([this, nextChildName, path = std::move(path)](
                       auto &&attr) { return getFileAttr(attr.uuid(), path); });
}

folly::Future<std::shared_ptr<FuseFileHandle>> S3Logic::open(
    std::string requestId, const folly::fbstring &spaceId, const FileAttr &attr,
    const size_t requestedOffset, const int flags, const size_t requestedSize)
{
    LOG_FCALL() << LOG_FARG(spaceId) << LOG_FARGH(flags);

    {
        std::lock_guard<std::mutex> lockGuard{m_handleMutex};

        if (m_requestHandles.find(requestId) != m_requestHandles.end())
            return m_requestHandles.at(requestId);
    }

    const auto filteredFlags = flags;
    const auto flag = detail::getOpenFlag(helpers::maskToFlags(filteredFlags));
    OpenFile msg{attr.uuid().toStdString(), flag};

    LOG_DBG(2) << "Sending file opened message for " << attr.uuid();

    return communicate<FileOpened>(std::move(msg))
        .via(m_executor.get())
        .thenTry([this, filteredFlags, flags, attr, requestedOffset,
                     requestedSize, requestId](auto &&opened) {
            if (opened.hasException()) {
                opened.throwIfFailed();
            }

            {
                std::lock_guard<std::mutex> lockGuard{m_handleMutex};

                m_requestHandles.emplace(requestId,
                    std::make_shared<FuseFileHandle>(filteredFlags,
                        opened.value().handleId(),
                        std::shared_ptr<client::cache::OpenFileMetadataCache::
                                OpenFileToken>{},
                        m_helpersCache, m_forceProxyIOCache,
                        m_providerTimeout));
                m_requestFileHandleFlags.emplace(requestId, flags);

                LOG_DBG(2) << "Assigned fuse handle " << requestId
                           << " for file " << attr.uuid();
            }

            return ensureFileLocationForRange(
                attr, requestedOffset, requestedSize);
        })
        .thenTry([this, spaceId, requestedOffset, requestedSize, attr,
                     requestId](auto &&fileLocation) {
            if (fileLocation.hasException()) {
                LOG(ERROR) << "Failed to prefetch file range "
                           << requestedOffset << ":" << requestedSize
                           << "due to: " << fileLocation.exception().what();
                fileLocation.throwIfFailed();
            }

            S3RequestContext context{};
            context.spaceId = spaceId;
            context.location = std::move(fileLocation.value());
            context.attr = attr;
            context.offset = 0UL;

            std::lock_guard<std::mutex> lockGuard{m_handleMutex};
            m_requestContext.emplace(requestId, std::move(context));

            return m_requestHandles.at(requestId);
        });
}

folly::Future<FileAttr> S3Logic::create(std::string requestId,
    const folly::fbstring &parentUuid, const folly::fbstring &path,
    const mode_t mode, const int flags)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(path) << LOG_FARG(mode)
                << LOG_FARG(flags);

    if (S_ISDIR(mode) || S_ISCHR(mode) || S_ISBLK(mode) || S_ISFIFO(mode) ||
        S_ISLNK(mode) || S_ISSOCK(mode)) {

        LOG(ERROR) << "Attempt to create unsupported file type - only regular "
                      "files are supported for this call.";
        return folly::makeFutureWith([]() -> FileAttr {
            throw std::errc::operation_not_supported; // NOLINT
        });
    }

    return folly::makeSemiFutureWith([this, path, parentUuid]() {
        std::vector<std::string> pathTokens;
        folly::split("/", path, pathTokens, false);

        if (pathTokens.size() > 1) {
            std::vector<std::string> pathParentTokens = pathTokens;
            pathParentTokens.pop_back();
            CreatePath msg{parentUuid,
                fmt::format("{}", fmt::join(pathParentTokens, "/"))};

            return folly::collectAll(communicate<FileAttr>(std::move(msg))
                                         .thenValue([](auto &&parentAttr) {
                                             return parentAttr.uuid();
                                         }),
                folly::makeFuture(pathTokens.back()));
        }

        PUSH_FUTURES_2(parentUuid, pathTokens.back());
    })
        .via(m_executor.get())
        .thenValue([this, mode, flags](auto &&args) {
            POP_FUTURES_2(args, effectiveParentUuid, name);

            constexpr auto modeMask =
                S_ISUID | S_ISGID | S_ISVTX | S_IRWXU | S_IRWXG | S_IRWXO;
            const auto flag = detail::getOpenFlag(helpers::maskToFlags(flags));

            return communicate<FileCreated>(
                CreateFile{effectiveParentUuid.value(), name.value(),
                    mode & modeMask, flag});
        })
        .thenValue([this, path, flags, parentUuid, requestId](auto &&created) {
            const auto &uuid = created.attr().uuid();

            S3RequestContext context{};
            context.spaceId = parentUuid;
            context.location = created.location();
            context.attr = created.attr();
            context.offset = 0;

            auto fuseFileHandle =
                std::make_shared<FuseFileHandle>(flags, created.handleId(),
                    std::shared_ptr<
                        client::cache::OpenFileMetadataCache::OpenFileToken>{},
                    m_helpersCache, m_forceProxyIOCache, m_providerTimeout);

            {
                std::lock_guard<std::mutex> lockGuard{m_handleMutex};
                m_requestContext.emplace(requestId, std::move(context));
                m_requestFileHandleFlags.emplace(requestId, flags);
                m_requestHandles.emplace(requestId, std::move(fuseFileHandle));
            }

            LOG_DBG(2) << "Created object " << path << " with uuid " << uuid;

            return created.attr();
        });
}

folly::Future<folly::Unit> S3Logic::releaseFileHandle(
    const folly::fbstring &uuid, std::shared_ptr<FuseFileHandle> fileHandle)
{
    folly::fbvector<folly::Future<folly::Unit>> releaseFutures;
    for (auto &helperHandle : fileHandle->helperHandles())
        releaseFutures.emplace_back(helperHandle->release());

    return folly::collectAll(std::move(releaseFutures))
        .via(m_executor.get())
        .thenValue([this, fileHandle, uuid](auto && /*futs*/) {
            return communicate(Release{uuid.toStdString(),
                fileHandle->providerHandleId()->toStdString()});
        })
        .thenValue([](auto && /*status*/) { return folly::makeFuture(); });
}

folly::Future<FileAttr> S3Logic::getFileAttrByPath(
    const folly::fbstring &bucketId, const folly::fbstring &path)
{
    return communicate<FileAttr>(
        GetFileAttrByPath{bucketId.toStdString(), path.toStdString(), {}});
}

folly::Future<FileAttr> S3Logic::getFileParentAttrByPath(
    const FileAttr &bucketAttr, const folly::fbstring &path)
{
    auto parentAttr = bucketAttr;
    std::vector<std::string> pathTokens;
    folly::split("/", path, pathTokens, false);
    if (pathTokens.size() == 1) {
        return folly::makeFuture<FileAttr>(std::move(parentAttr));
    }

    auto pathParentTokens = pathTokens;
    pathParentTokens.pop_back();
    auto parentPath = fmt::format("{}", fmt::join(pathParentTokens, "/"));
    return communicate<FileAttr>(CreatePath{bucketAttr.uuid(), parentPath});
}

Aws::S3::Model::ListBucketsResult S3Logic::toListBucketsResult(
    FileChildrenAttrs &&msg)
{
    Aws::Vector<Aws::S3::Model::Bucket> buckets;

    for (const auto &child : msg.childrenAttrs()) {
        Aws::S3::Model::Bucket bucket;
        bucket.SetName(child.name().toStdString());
        bucket.SetCreationDate(child.mtime());
        buckets.emplace_back(std::move(bucket));
    }

    Aws::S3::Model::ListBucketsResult result;
    result.SetBuckets(std::move(buckets));

    return result;
}

Aws::S3::Model::CreateMultipartUploadResult
S3Logic::toCreateMultipartUploadResult(MultipartUpload &&msg,
    const folly::fbstring &bucket, const folly::fbstring &path)
{
    Aws::S3::Model::CreateMultipartUploadResult result;
    result.SetKey(path.toStdString());
    result.SetUploadId(msg.id());
    result.SetBucket(bucket.toStdString());
    return result;
}
} // namespace s3
} // namespace one