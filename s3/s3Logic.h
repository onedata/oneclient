/**
 * @file s3Logic.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "auth/macaroonHandler.h"
#include "cache/forceProxyIOCache.h"
#include "cache/helpersCache.h"
#include "cache/openFileMetadataCache.h"
#include "configuration.h"
#include "context.h"
#include "events/types/fileWritten.h"
#include "fslogic/fuseFileHandle.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"
#include "messages/fuse/uuid.h"
#include "messages/fuse/xattr.h"
#include "onepanelRestClient.h"
#include "options/options.h"
#include "s3Exception.h"
#include "util/md5.h"
#include "util/mime.h"
#include "util/uuid.h"
#include "util/xattrHelper.h"

#include <RangeParser.h>
#include <aws/s3/model/AbortMultipartUploadResult.h>
#include <aws/s3/model/CompleteMultipartUploadResult.h>
#include <aws/s3/model/CompletedMultipartUpload.h>
#include <aws/s3/model/CreateMultipartUploadResult.h>
#include <aws/s3/model/GetObjectResult.h>
#include <aws/s3/model/HeadObjectResult.h>
#include <aws/s3/model/ListBucketsResult.h>
#include <aws/s3/model/ListMultipartUploadsResult.h>
#include <aws/s3/model/ListObjectsResult.h>
#include <aws/s3/model/ListObjectsV2Result.h>
#include <aws/s3/model/ListPartsResult.h>
#include <aws/s3/model/UploadPartResult.h>
#include <drogon/utils/Utilities.h>
#include <folly/concurrency/ConcurrentHashMap.h>
#include <folly/futures/Future.h>
#include <folly/futures/SharedPromise.h>

#include <atomic>
#include <chrono>
#include <memory>

namespace one {
namespace s3 {

namespace {
template <int N, typename T> auto farg(T &&arg)
{
    return folly::makeFuture(std::forward<T>(arg));
}
} // namespace

namespace detail {
/**
 * Filters given flags set to one of RDONLY, WRONLY or RDWR.
 * Returns RDONLY if flag value is zero.
 * @param Flags value
 */
inline helpers::Flag getOpenFlag(const helpers::FlagsSet &flagsSet)
{
    if (flagsSet.count(one::helpers::Flag::RDONLY) > 0)
        return one::helpers::Flag::RDONLY;
    if (flagsSet.count(one::helpers::Flag::WRONLY) > 0)
        return one::helpers::Flag::WRONLY;
    if (flagsSet.count(one::helpers::Flag::RDWR) > 0)
        return one::helpers::Flag::RDWR;

    return one::helpers::Flag::RDONLY;
}
} // namespace detail

struct S3RequestContext {
    folly::fbstring spaceId{};
    one::messages::fuse::FileAttr attr;
    one::messages::fuse::FileLocation location;
    std::size_t offset{};
};

const folly::fbstring ONEDATA_S3_MULTIPART_PREFIX{"__ones3__"};
const folly::fbstring ONEDATA_S3_MULTIPART_PREFIX_OLD{".__s3__mpus__"};

const std::string ONEDATA_S3_XATTR_CONTENT_MD5{"s3.content.md5"};
const std::string ONEDATA_S3_XATTR_CONTENT_TYPE{"s3.content.type"};

folly::fbstring getFileNameFromPath(const folly::fbstring &path);

folly::fbstring getMultipartUploadTemporaryFileName(
    const folly::fbstring &key, const folly::fbstring &uploadId);

folly::fbstring getCompleteUploadTemporaryFileName(
    const std::string &requestId);

folly::fbstring getMultipartUploadTemporaryDir(const folly::fbstring &uploadId);

class S3Logic : public std::enable_shared_from_this<S3Logic> {
public:
    S3Logic(std::shared_ptr<one::client::options::Options> options,
        folly::fbstring token,
        std::shared_ptr<folly::IOThreadPoolExecutor> executor);

    folly::Future<std::shared_ptr<S3Logic>> connect();

    folly::Future<Aws::S3::Model::ListBucketsResult> listBuckets();

    folly::Future<Aws::S3::Model::CreateMultipartUploadResult>
    createMultipartUpload(const folly::fbstring &bucket,
        const folly::fbstring &path, const folly::fbstring &requestId,
        const folly::fbstring &contentType);

    folly::Future<Aws::S3::Model::AbortMultipartUploadResult>
    abortMultipartUpload(
        const folly::fbstring &bucket, const folly::fbstring &uploadId);

    folly::Future<Aws::S3::Model::UploadPartResult> uploadMultipartPart(
        const std::string &requestId, const folly::fbstring &bucket,
        const folly::fbstring &path, const folly::fbstring &uploadId,
        const size_t partNumber, const size_t partSize,
        const folly::fbstring &partMD5, std::shared_ptr<folly::IOBuf> buf);

    folly::Future<Aws::S3::Model::CompleteMultipartUploadResult>
    completeMultipartUpload(const std::string requestId,
        const folly::fbstring &bucket, const folly::fbstring &path,
        const folly::fbstring &uploadId);

    folly::Future<Aws::S3::Model::ListPartsResult> listMultipartUploadParts(
        const folly::fbstring &uploadId, const folly::fbstring &bucket,
        const folly::fbstring &path, size_t maxParts,
        folly::Optional<size_t> partMarker);

    folly::Future<Aws::S3::Model::ListMultipartUploadsResult>
    listMultipartUploads(const folly::fbstring &bucket, size_t maxUploads,
        const folly::Optional<folly::fbstring> &indexToken);

    folly::Future<std::pair<Aws::S3::Model::HeadObjectResult,
        std::pair<std::function<std::size_t(char *, std::size_t)>, std::string>>>
    getObject(const folly::fbstring &bucket, const folly::fbstring &path,
        const std::string &requestId,
        const folly::Optional<folly::fbstring> &rangeHeader,
        std::function<void(size_t)> completionCallback);

    folly::Future<Aws::S3::Model::ListObjectsV2Result> readDirV2Recursive(
        const folly::fbstring &bucket, const folly::fbstring &prefix,
        const folly::Optional<folly::fbstring> &marker, const size_t maxKeys,
        bool includeDirectories = true);

    folly::Future<Aws::S3::Model::ListObjectsResult> readDirRecursive(
        const folly::fbstring &bucket, const folly::fbstring &prefix,
        const folly::Optional<folly::fbstring> &marker, const size_t maxKeys);

    folly::Future<Aws::S3::Model::ListObjectsResult> readDir(
        const folly::fbstring &bucket, const folly::fbstring &prefix,
        const folly::Optional<folly::fbstring> &marker,
        const folly::fbstring &delimiter, const size_t maxKeys);

    folly::Future<Aws::S3::Model::ListObjectsV2Result> readDirV2(
        const folly::fbstring &bucket, const folly::fbstring &prefix,
        const folly::Optional<folly::fbstring> &marker,
        const folly::fbstring &delimiter, const size_t maxKeys);

    folly::Future<Aws::S3::Model::HeadObjectResult> headBucket(
        const folly::fbstring &bucket, const folly::fbstring &requestId);

    folly::Future<Aws::S3::Model::HeadObjectResult> headObject(
        const folly::fbstring &bucket, const folly::fbstring &path,
        const std::string &requestId);

    folly::Future<folly::Unit> close(
        const folly::fbstring &uuid, const std::string &requestId);

    S3RequestContext &getRequestContext(const std::string &requestId);

    folly::Future<std::size_t> write(
        std::shared_ptr<one::client::fslogic::FuseFileHandle> fileHandle,
        folly::fbstring uuid, const std::string &requestId,
        std::shared_ptr<folly::IOBuf> buf, const size_t baseOffset = 0);

    folly::Future<folly::IOBufQueue> read(
        std::shared_ptr<one::client::fslogic::FuseFileHandle> fileHandle,
        const folly::fbstring &spaceId,
        const one::messages::fuse::FileAttr &attr, const std::size_t offset,
        const std::size_t size);

    folly::Future<folly::IOBufQueue> read(
        std::shared_ptr<one::client::fslogic::FuseFileHandle> fileHandle,
        const std::string &requestId, const std::size_t size,
        const std::size_t requestOffset, const std::size_t requestSize,
        const folly::Optional<folly::fbstring> &checksum = {});

    folly::Future<one::messages::fuse::Uuid> resolveGuid(
        const folly::fbstring &path);

    folly::Future<one::messages::fuse::FileAttr> getFileAttr(
        const folly::fbstring &spaceId, const folly::fbstring &path);

    folly::Future<one::messages::fuse::FileAttr> getBucketAttr(
        const folly::fbstring &bucket);

    folly::Future<one::messages::fuse::FileAttr> getFileAttr(
        const folly::fbstring &parentId, std::vector<std::string> path);

    folly::Future<size_t> uploadObject(const std::string &requestId,
        const folly::fbstring &bucket, const folly::fbstring &path,
        const folly::fbstring &md5, const folly::fbstring &contentType,
        std::shared_ptr<folly::IOBuf> buf);

    folly::Future<folly::Unit> deleteObject(const std::string &requestId,
        const folly::fbstring &bucket, const folly::fbstring &path);

    folly::Future<std::shared_ptr<one::client::fslogic::FuseFileHandle>> open(
        std::string requestId, const folly::fbstring &spaceId,
        const one::messages::fuse::FileAttr &attr, const size_t offset,
        const int flags);

    folly::Future<one::messages::fuse::FileAttr> create(std::string requestId,
        const folly::fbstring &parentUuid, const folly::fbstring &path,
        const mode_t mode, const int flags);

    folly::Future<folly::Unit> releaseFileHandle(const folly::fbstring &uuid,
        std::shared_ptr<client::fslogic::FuseFileHandle> fileHandle);

    folly::Future<messages::fuse::FileAttr> getFileAttrByPath(
        const folly::fbstring &bucketId, const folly::fbstring &path);

    size_t getOpenFileCount()
    {
        std::lock_guard<std::mutex> l{m_handleMutex};
        return m_requestHandles.size();
    }

    size_t getThreadPoolActiveThreads()
    {
        return m_executor->numActiveThreads();
    }

    bool isConnected()
    {
        return m_context && m_context->communicator() &&
            m_context->communicator()->isConnected();
    }

    size_t getDownloadedBytes() { return m_downloadedBytes.load(); }

    size_t getUploadedBytes() { return m_uploadedBytes.load(); }

private:
    template <typename SrvMsg = one::messages::fuse::FuseResponse,
        typename CliMsg>
    folly::Future<SrvMsg> communicate(
        CliMsg &&msg, const std::chrono::seconds timeout)
    {
        auto messageString = msg.toString();
        return m_context->communicator()
            ->communicate<SrvMsg>(std::forward<CliMsg>(msg))
            .via(m_executor.get())
            .onTimeout(timeout,
                [messageString = std::move(messageString),
                    timeout = timeout.count()]() {
                    LOG(ERROR)
                        << "Response to message : " << messageString
                        << " not received within " << timeout << " seconds.";
                    return folly::makeFuture<SrvMsg>(std::system_error{
                        std::make_error_code(std::errc::timed_out)});
                });
    }

    folly::Future<messages::fuse::FileAttr> getFileParentAttrByPath(
        const messages::fuse::FileAttr &bucketAttr,
        const folly::fbstring &path);

    std::shared_ptr<client::auth::AuthManager> m_authManager;
    std::shared_ptr<one::client::Context> m_context;

    std::shared_ptr<one::messages::Configuration> m_configuration;
    client::cache::HelpersCacheThreadSafeAdapter m_helpersCache;

    const std::chrono::seconds m_providerTimeout;

    std::shared_ptr<one::client::options::Options> m_options;

    folly::fbstring m_rootUuid;

    bool m_connected;

    folly::fbstring m_token;

    std::mutex m_handleMutex;

    std::unordered_map<std::string,
        std::shared_ptr<one::client::fslogic::FuseFileHandle>>
        m_requestHandles;
    std::unordered_map<std::string, int> m_requestFileHandleFlags;
    std::unordered_map<std::string, S3RequestContext> m_requestContext;
    std::unordered_map<std::string, folly::fbstring> m_fuseDirectoryHandles;
    client::cache::ForceProxyIOCache m_forceProxyIOCache;
    const unsigned int m_randomReadPrefetchEvaluationFrequency{1000};

    folly::ConcurrentHashMap<folly::fbstring, one::messages::fuse::FileAttr>
        m_bucketIdCache;

    std::shared_ptr<folly::IOThreadPoolExecutor> m_executor;

    std::atomic<size_t> m_uploadedBytes{0};
    std::atomic<size_t> m_downloadedBytes{0};
};

class S3LogicCache {
public:
    S3LogicCache() = default;

    S3LogicCache(std::shared_ptr<one::client::options::Options> options);

    folly::Future<std::shared_ptr<S3Logic>> get(const folly::fbstring &token);

    bool updateClientStatus(Poco::JSON::Array &clients)
    {
        bool isOk{true};
        std::lock_guard<std::mutex> l{m_cacheMutex};
        for (auto &it : m_cache) {
            const auto key = it.first;
            auto s3Logic = it.second->getFuture();

            if (!s3Logic.isReady())
                continue;

            if (s3Logic.hasException())
                continue;

            Poco::JSON::Object client;
            client.set("id", key.toStdString());

            client.set("isConnected", s3Logic.value()->isConnected());
            client.set("openFileCount", s3Logic.value()->getOpenFileCount());
            client.set(
                "downloadedBytes", s3Logic.value()->getDownloadedBytes());
            client.set("uploadedBytes", s3Logic.value()->getUploadedBytes());
            client.set("activeWorkerThreads",
                s3Logic.value()->getThreadPoolActiveThreads());

            if (!s3Logic.value()->isConnected())
                isOk = false;

            clients.add(std::move(client));
        }

        return isOk;
    }

private:
    std::shared_ptr<one::client::options::Options> m_options;
    bool m_initialized{false};

    mutable std::mutex m_cacheMutex;
    std::unordered_map<folly::fbstring,
        std::shared_ptr<folly::SharedPromise<std::shared_ptr<S3Logic>>>>
        m_cache;

    std::shared_ptr<folly::IOThreadPoolExecutor> m_executor;
};
} // namespace s3
} // namespace one