/**
 * @file s3Logic.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Logic.h"

#include "messages/fuse/abortMultipartUpload.h"
#include "messages/fuse/completeMultipartUpload.h"
#include "messages/fuse/createFile.h"
#include "messages/fuse/createMultipartUpload.h"
#include "messages/fuse/createPath.h"
#include "messages/fuse/deleteFile.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileChildrenAttrs.h"
#include "messages/fuse/fileCreated.h"
#include "messages/fuse/fileList.h"
#include "messages/fuse/fileOpened.h"
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/fsync.h"
#include "messages/fuse/getChildAttr.h"
#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/getFileAttrByPath.h"
#include "messages/fuse/getFileChildrenAttrs.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/getXAttr.h"
#include "messages/fuse/listFilesRecursively.h"
#include "messages/fuse/listMultipartParts.h"
#include "messages/fuse/listMultipartUploads.h"
#include "messages/fuse/multipartParts.h"
#include "messages/fuse/multipartUpload.h"
#include "messages/fuse/multipartUploads.h"
#include "messages/fuse/openFile.h"
#include "messages/fuse/release.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/reportFileWritten.h"
#include "messages/fuse/resolveGuid.h"
#include "messages/fuse/setXAttr.h"
#include "messages/fuse/truncate.h"
#include "messages/fuse/uploadMultipartPart.h"
#include "monitoring/monitoring.h"

#include <spdlog/spdlog.h>

namespace one {
namespace s3 {

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
    , m_executor{std::move(executor)}
{
    m_context = std::make_shared<one::client::Context>();
    m_context->setScheduler(
        std::make_shared<Scheduler>(m_options->getSchedulerThreadCount()));
    m_context->setOptions(m_options);
}

folly::Future<std::shared_ptr<S3Logic>> S3Logic::connect()
{
    if (m_connected)
        return shared_from_this();

    m_authManager = one::client::getOptionsAuthManager(m_context);
    auto sessionId = one::client::generateSessionId();
    m_configuration = one::client::getConfiguration(sessionId, m_authManager,
        m_context, messages::handshake::ClientType::ones3);

    auto communicator = getCommunicator(sessionId, m_authManager, m_context,
        messages::handshake::ClientType::ones3);
    m_context->setCommunicator(communicator);
    communicator->connect();
    communicator->schedulePeriodicMessageRequest();
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

    m_helpersCache.setCache(std::make_unique<one::client::cache::HelpersCache>(
        *communicator, m_context->scheduler(), *m_context->options()));

    m_rootUuid = m_configuration->rootUuid();

    m_connected = true;

    return folly::makeSemiFuture(shared_from_this()).via(m_executor.get());
}

folly::Future<Aws::S3::Model::ListBucketsResult> S3Logic::listBuckets()
{
    folly::Optional<folly::fbstring> indexToken;
    constexpr auto kMaxFetchSize{10000};

    return communicate<one::messages::fuse::FileChildrenAttrs>(
        one::messages::fuse::GetFileChildrenAttrs{
            m_rootUuid, 0, kMaxFetchSize, indexToken, false, false},
        m_providerTimeout)
        .thenValue([](auto &&msg) {
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
        });
}

folly::Future<Aws::S3::Model::CreateMultipartUploadResult>
S3Logic::createMultipartUpload(const folly::fbstring &bucket,
    const folly::fbstring &path, const folly::fbstring &requestId,
    const folly::fbstring &contentType)
{
    folly::Optional<folly::fbstring> indexToken;

    assert(!bucket.empty());
    assert(!path.empty());

    return getBucketAttr(bucket)
        //
        // Create multipart upload on the server
        //
        .thenTry([path, bucket, requestId, this](
                     folly::Try<messages::fuse::FileAttr> &&bucketAttr) {
            if (bucketAttr.hasException()) {
                throw one::s3::error::NoSuchBucket{bucket.toStdString(),
                    path.toStdString(), requestId.toStdString()};
            }

            const auto &spaceId = bucketAttr.value().uuid();
            auto arg0 = folly::makeFuture(std::move(bucketAttr));
            auto arg1 = communicate<one::messages::fuse::MultipartUpload>(
                one::messages::fuse::CreateMultipartUpload{
                    one::client::util::uuid::uuidToSpaceId(spaceId)
                        .toStdString(),
                    path.toStdString()},
                m_providerTimeout);

            return folly::collectAll(std::move(arg0), std::move(arg1));
        })
        //
        // Create temporary upload directory
        //
        .thenValue([bucket, path, this](auto &&args) {
            auto &bucketAttr = std::get<0>(args);
            auto &upload = std::get<1>(args).value();

            const auto temporaryUploadPath =
                getMultipartUploadTemporaryDir(upload.id());

            auto arg0 = folly::makeFuture(std::move(upload));
            auto arg1 = communicate<messages::fuse::FileAttr>(
                messages::fuse::CreatePath{
                    bucketAttr.value().uuid(), temporaryUploadPath},
                m_providerTimeout);

            return folly::collectAll(std::move(arg0), std::move(arg1));
        })
        //
        // Set the content-type metadata on the directory
        //
        .thenValue([bucket, path, contentType, this](auto &&args) {
            auto &upload = std::get<0>(args).value();
            auto &uploadTmpDirAttr = std::get<1>(args).value();

            auto arg0 = folly::makeFuture(std::move(upload));
            auto arg1 = communicate<messages::fuse::FuseResponse>(
                messages::fuse::SetXAttr{uploadTmpDirAttr.uuid(),
                    ONEDATA_S3_XATTR_CONTENT_TYPE,
                    fmt::format("\"{}\"", contentType), false, false},
                m_providerTimeout);

            return folly::collectAll(std::move(arg0), std::move(arg1));
        })
        //
        // Generate CreateMultipartUploadResult response
        //
        .thenValue([bucket, path](auto &&args) {
            auto &upload = std::get<0>(args).value();

            Aws::S3::Model::CreateMultipartUploadResult result;
            result.SetKey(path.toStdString());
            result.SetUploadId(upload.id());
            result.SetBucket(bucket.toStdString());
            return result;
        });
}

folly::Future<Aws::S3::Model::AbortMultipartUploadResult>
S3Logic::abortMultipartUpload(
    const folly::fbstring &bucket, const folly::fbstring &uploadId)
{
    folly::Optional<folly::fbstring> indexToken;

    assert(!bucket.empty());
    assert(!uploadId.empty());

    constexpr auto kMaxListSize{10000};

    return getBucketAttr(bucket)
        //
        // Get the uuid of the temporary upload directory
        //
        .thenValue([this, uploadId](messages::fuse::FileAttr &&bucketAttr) {
            const auto temporaryUploadPath =
                getMultipartUploadTemporaryDir(uploadId);

            return communicate<messages::fuse::FileAttr>(
                messages::fuse::GetFileAttrByPath{
                    bucketAttr.uuid(), temporaryUploadPath, {}},
                m_providerTimeout);
        })
        //
        // List the temporary upload files
        //
        .thenValue([this](messages::fuse::FileAttr &&attr) {
            return communicate<one::messages::fuse::FileChildrenAttrs>(
                one::messages::fuse::GetFileChildrenAttrs{
                    attr.uuid().toStdString(), 0, kMaxListSize},
                m_providerTimeout);
        })
        //
        // Remove the temporary upload files
        //
        .thenValue([this](messages::fuse::FileChildrenAttrs &&attrs) {
            std::vector<folly::Future<one::messages::fuse::FuseResponse>> futs;
            for (const auto &attr : attrs.childrenAttrs()) {
                futs.emplace_back(communicate(
                    messages::fuse::DeleteFile{attr.uuid().toStdString()},
                    m_providerTimeout));
            }
            return futs;
        })
        //
        // Abort multipart upload
        //
        .thenValue([this, uploadId](auto && /*response*/) {
            return communicate(
                one::messages::fuse::AbortMultipartUpload{
                    uploadId.toStdString()},
                m_providerTimeout);
        })
        //
        // Generate unit response on success
        //
        .thenValue([](auto && /*response*/) {
            Aws::S3::Model::AbortMultipartUploadResult result;
            return result;
        });
}

folly::Future<Aws::S3::Model::UploadPartResult> S3Logic::uploadMultipartPart(
    const std::string &requestId, const folly::fbstring &bucket,
    const folly::fbstring &path, const folly::fbstring &uploadId,
    const size_t partNumber, const size_t partSize,
    const folly::fbstring &partMD5, std::shared_ptr<folly::IOBuf> buf)
{

    return getBucketAttr(bucket)
        //
        // Get path to the temporary upload directory and file name
        //
        .thenValue([this, requestId, partSize, path, uploadId](
                       auto &&bucketAttr) {
            constexpr auto kDefaultFilePerms{0655};

            const auto tmpPath = fmt::format("{}-{}",
                getMultipartUploadTemporaryFileName(path, uploadId)
                    .toStdString(),
                partSize);

            auto arg0 = folly::makeFuture(bucketAttr);

            auto arg1 = getFileAttr(bucketAttr.uuid(), tmpPath)
                            .thenError(folly::tag_t<std::system_error>{},
                                [this, tmpPath, parentUuid = bucketAttr.uuid(),
                                    requestId, path](auto && /*e*/) {
                                    return create(requestId, parentUuid,
                                        tmpPath, S_IFREG, kDefaultFilePerms);
                                })
                            .thenError(folly::tag_t<std::system_error>{},
                                [this, tmpPath, parentUuid = bucketAttr.uuid(),
                                    requestId, path](auto && /*e*/) {
                                    // sic
                                    return getFileAttr(parentUuid, tmpPath);
                                });

            auto arg2 = folly::makeFuture(tmpPath);

            return folly::collectAll(
                std::move(arg0), std::move(arg1), std::move(arg2));
        })
        //
        // Open the part specific file
        //
        .thenValue([this, bucket, path, uploadId, requestId](auto &&args) {
            auto &bucketAttr = std::get<0>(args).value();
            auto &tmpPath = std::get<2>(args).value();
            auto &attr = std::get<1>(args);
            if (attr.hasException()) {
                LOG_DBG(1) << "Failed to create multipart upload part file "
                           << tmpPath;
                throw one::s3::error::InternalServerError{bucket.toStdString(),
                    path.toStdString(), uploadId.toStdString()};
            }

            auto arg0 = folly::makeFuture(bucketAttr);
            auto arg1 = folly::makeFuture(attr);
            auto arg2 =
                open(requestId, bucketAttr.uuid(), attr.value(), 0UL, O_WRONLY);
            auto arg3 = folly::makeFuture(tmpPath);

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3));
        })
        //
        // Write the part contents to the temporary file
        //
        .thenValue([this, requestId, partSize, partNumber,
                       buf = std::move(buf)](auto &&args) mutable {
            auto &attr = std::get<1>(args).value();
            auto &fileHandle = std::get<2>(args).value();

            auto arg0 = folly::makeFuture(fileHandle);
            auto arg1 = folly::makeFuture(attr);
            auto arg2 = write(fileHandle, attr.uuid(), requestId,
                std::move(buf), (partNumber - 1) * partSize);

            return folly::collectAll(
                std::move(arg0), std::move(arg1), std::move(arg2));
        })
        //
        // Close part file
        //
        .thenValue([this, requestId](auto &&args) {
            auto &fileUuid = std::get<1>(args).value().uuid();
            const auto written = std::get<2>(args).value();

            m_uploadedBytes.fetch_add(written);

            return close(fileUuid, requestId);
        })
        //
        // Report multipart part upload complete
        //
        .thenValue(
            [this, uploadId, partMD5, partNumber, partSize](auto && /*unit*/) {
                return communicate(
                    messages::fuse::UploadMultipartPart{uploadId.toStdString(),
                        partMD5.toStdString(),
                        static_cast<uint64_t>(std::time(nullptr)), partNumber,
                        partSize},
                    m_providerTimeout);
            })
        //
        // Generate UploadPartResult response
        //
        .thenValue([partMD5](auto && /*unit*/) {
            Aws::S3::Model::UploadPartResult result;
            result.SetETag(fmt::format("\"{}\"", partMD5.toStdString()));
            return result;
        });
}

folly::Future<Aws::S3::Model::CompleteMultipartUploadResult>
S3Logic::completeMultipartUpload(const std::string requestId,
    const folly::fbstring &bucket, const folly::fbstring &path,
    const folly::fbstring &uploadId)
{
    assert(!bucket.empty());
    assert(!path.empty());
    assert(!uploadId.empty());

    LOG_DBG(2) << "Completing multipart upload " << uploadId;

    constexpr auto kMaxPartsList{10000};
    auto arg0 = getBucketAttr(bucket);
    auto arg1 =
        listMultipartUploadParts(uploadId, bucket, path, kMaxPartsList, {0});

    return folly::collectAll(std::move(arg0), std::move(arg1))
        .via(folly::getGlobalCPUExecutor().get())
        //
        // Determine the attributes of the first part and last part
        //
        .thenValue([this, uploadId, path](auto &&args) {
            const auto &bucketAttr = std::get<0>(args);
            const auto &parts = std::get<1>(args);
            auto spaceId = bucketAttr.value().uuid();

            const auto firstPartSize =
                parts.value().GetParts().front().GetSize();
            const auto lastPartSize = parts.value().GetParts().back().GetSize();
            const auto lastPartNumber =
                parts.value().GetParts().back().GetPartNumber();

            const auto firstPartPath = fmt::format("{}-{}",
                getMultipartUploadTemporaryFileName(path, uploadId)
                    .toStdString(),
                firstPartSize);
            const auto lastPartPath = fmt::format("{}-{}",
                getMultipartUploadTemporaryFileName(path, uploadId)
                    .toStdString(),
                lastPartSize);

            // calculate the target file Etag
            std::stringstream multipartETagStream;
            LOG_DBG(3) << "Completing multipart upload from parts:";
            for (const auto &part : parts.value().GetParts()) {
                LOG_DBG(3) << " part number: " << part.GetPartNumber()
                           << " part size: " << part.GetSize()
                           << " part etag: " << part.GetETag();
                const std::string &partETag = part.GetETag();
                multipartETagStream << partETag.substr(1, partETag.size() - 2);
            }

            std::string multipartETag;
            folly::unhexlify(multipartETagStream.str(), multipartETag);
            auto multipartETagMd5 =
                one::client::util::md5::md5(
                    multipartETag.data(), multipartETag.size()) +
                "-" + std::to_string(parts.value().GetParts().size());

            auto arg0 = folly::makeFuture(std::move(bucketAttr));
            auto arg1 =
                getFileAttrByPath(bucketAttr.value().uuid(), firstPartPath);
            auto arg2 =
                getFileAttrByPath(bucketAttr.value().uuid(), lastPartPath);
            auto arg3 = folly::makeFuture(lastPartNumber);
            auto arg4 = folly::makeFuture(lastPartSize);
            auto arg5 = folly::makeFuture(firstPartSize);
            auto arg6 = folly::makeFuture(std::move(multipartETagMd5));

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3), std::move(arg4),
                std::move(arg5), std::move(arg6));
        })
        //
        // Open first part for writing to append remaining parts to the end
        // of it
        // TODO: Currently this code only handles cases where all parts
        //       except the last one have the same size
        //
        .thenValue([this, requestId](auto &&futs) {
            auto &bucketAttr = std::get<0>(futs);
            auto &firstPartAttr = std::get<1>(futs);
            auto &lastPartAttr = std::get<2>(futs);
            const auto lastPartNumber = std::get<3>(futs).value();
            const auto lastPartSize = std::get<4>(futs).value();
            const auto firstPartSize = std::get<5>(futs).value();
            auto &multipartETagMd5 = std::get<6>(futs);

            const bool isLastPartSizeEqualFirst =
                firstPartAttr.value().uuid() == lastPartAttr.value().uuid();

            auto arg0 = folly::makeFuture(std::move(bucketAttr));
            auto arg1 = folly::makeFuture(std::move(firstPartAttr));
            auto arg2 = folly::makeFuture(std::move(lastPartAttr));
            auto arg3 = folly::makeFuture(lastPartNumber);
            auto arg4 = folly::makeFuture(lastPartSize);
            auto arg5 = folly::makeFuture(firstPartSize);
            auto arg6 = isLastPartSizeEqualFirst
                ? folly::makeFuture(
                      std::shared_ptr<one::client::fslogic::FuseFileHandle>{})
                : open(fmt::format("{}-{}", requestId, lastPartNumber),
                      bucketAttr.value().uuid(), lastPartAttr.value(), 0UL,
                      O_RDONLY);
            auto arg7 = communicate<messages::fuse::FuseResponse>(
                messages::fuse::SetXAttr{firstPartAttr.value().uuid(),
                    ONEDATA_S3_XATTR_CONTENT_MD5,
                    fmt::format("\"{}\"", multipartETagMd5.value()), false,
                    false},
                m_providerTimeout);
            auto arg8 = folly::makeFuture(multipartETagMd5);

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3), std::move(arg4),
                std::move(arg5), std::move(arg6), std::move(arg7),
                std::move(arg8));
        })
        //
        // Read the contents of the last part
        //
        .thenValue([this, requestId, bucket, path](auto &&args) {
            auto &bucketAttr = std::get<0>(args);
            auto &firstPartAttr = std::get<1>(args);
            auto &lastPartAttr = std::get<2>(args);
            auto lastPartNumber = std::get<3>(args).value();
            const auto lastPartSize = std::get<4>(args).value();
            const auto firstPartSize = std::get<5>(args).value();
            auto tmpLastPartFileHandle = std::get<6>(args).value();
            auto &multipartETagMd5 = std::get<8>(args);

            const auto spaceId = bucketAttr.value().uuid();
            const bool isLastPartSizeEqualFirst =
                firstPartAttr.value().uuid() == lastPartAttr.value().uuid();

            auto arg0 = folly::makeFuture(bucketAttr);
            auto arg1 = folly::makeFuture(firstPartAttr);
            auto arg2 = folly::makeFuture(lastPartAttr);
            auto arg3 = folly::makeFuture(lastPartNumber);
            auto arg4 = folly::makeFuture(firstPartSize);
            auto arg5 = isLastPartSizeEqualFirst
                ? folly::makeFuture(folly::IOBufQueue{})
                : read(tmpLastPartFileHandle,
                      fmt::format("{}-{}", requestId, lastPartNumber),
                      lastPartAttr.value(), lastPartSize * (lastPartNumber - 1),
                      lastPartSize);
            auto arg6 = isLastPartSizeEqualFirst
                ? folly::makeFuture(
                      std::shared_ptr<one::client::fslogic::FuseFileHandle>{})
                : open(fmt::format("{}-{}", requestId, 1),
                      bucketAttr.value().uuid(), firstPartAttr.value(), 0UL,
                      O_RDWR | O_APPEND);
            auto arg7 = folly::makeFuture(tmpLastPartFileHandle);
            auto arg8 = folly::makeFuture(multipartETagMd5);

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3), std::move(arg4),
                std::move(arg5), std::move(arg6), std::move(arg7),
                std::move(arg8));
        })
        //
        // Append the contents of the last part to the first part
        //
        .thenValue([this, requestId, bucket, path, uploadId](auto &&args) {
            auto &bucketAttr = std::get<0>(args);
            auto &firstPartAttr = std::get<1>(args);
            auto &lastPartAttr = std::get<2>(args);
            auto lastPartNumber = std::get<3>(args).value();
            const auto firstPartSize = std::get<4>(args).value();
            auto &bufQueue = std::get<5>(args).value();
            auto tmpTargetFileHandle = std::get<6>(args).value();
            auto tmpLastPartFileHandle = std::get<7>(args).value();
            auto &multipartETagMd5 = std::get<8>(args);

            const bool isLastPartSizeEqualFirst =
                firstPartAttr.value().uuid() == lastPartAttr.value().uuid();

            const auto firstPartPath = fmt::format("{}-{}",
                getMultipartUploadTemporaryFileName(path, uploadId)
                    .toStdString(),
                firstPartSize);

            auto arg0 = folly::makeFuture(bucketAttr);
            auto arg1 = folly::makeFuture(firstPartAttr);
            auto arg2 = folly::makeFuture(lastPartAttr);
            auto arg3 = isLastPartSizeEqualFirst
                ? folly::makeFuture<size_t>(0)
                : write(tmpTargetFileHandle, firstPartAttr.value().uuid(),
                      fmt::format("{}-{}", requestId, 1),
                      std::make_shared<folly::IOBuf>(bufQueue.moveAsValue()),
                      firstPartSize * (lastPartNumber - 1));
            auto arg4 = getFileAttrByPath(bucketAttr.value().uuid(),
                getMultipartUploadTemporaryDir(uploadId));
            auto arg5 = getFileParentAttrByPath(bucketAttr.value(), path);
            auto arg6 = isLastPartSizeEqualFirst
                ? folly::makeFuture()
                : close(lastPartAttr.value().uuid(),
                      fmt::format("{}-{}", requestId, lastPartNumber));
            auto arg7 = folly::makeFuture(tmpTargetFileHandle);
            auto arg8 = folly::makeFuture(multipartETagMd5);

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3), std::move(arg4),
                std::move(arg5), std::move(arg6), std::move(arg7),
                std::move(arg8));
        })
        //
        //
        // Rename the temporary complete file to target file
        //
        .thenValue([this, uploadId, path, requestId](auto &&args) {
            auto &bucketAttr = std::get<0>(args);
            auto &firstPartAttr = std::get<1>(args);
            auto &lastPartAttr = std::get<2>(args);
            auto &temporaryDirAttr = std::get<4>(args);
            auto &targetParentAttr = std::get<5>(args);
            auto tmpTargetFileHandle = std::get<7>(args).value();
            auto &multipartETagMd5 = std::get<8>(args);

            const auto &firstPartUuid =
                firstPartAttr.value().uuid().toStdString();
            const auto &newParentUuid =
                targetParentAttr.value().uuid().toStdString();
            const auto &newName = getFileNameFromPath(path).toStdString();

            auto contentType =
                one::client::util::mime::mimeFromPath(path.toStdString());

            const bool isLastPartSizeEqualFirst =
                firstPartAttr.value().uuid() == lastPartAttr.value().uuid();

            auto arg0 = folly::makeFuture(bucketAttr);
            auto arg1 = folly::makeFuture(firstPartAttr);
            auto arg2 = folly::makeFuture(lastPartAttr);
            auto arg3 = folly::makeFuture(temporaryDirAttr);
            auto arg4 = communicate<messages::fuse::FileRenamed>(
                messages::fuse::Rename{firstPartUuid, newParentUuid, newName},
                m_providerTimeout);
            auto arg5 = communicate<messages::fuse::FuseResponse>(
                messages::fuse::SetXAttr{firstPartUuid,
                    ONEDATA_S3_XATTR_CONTENT_TYPE,
                    fmt::format("\"{}\"", contentType), false, false},
                m_providerTimeout);
            auto arg6 = folly::makeFuture(multipartETagMd5);
            auto arg7 = isLastPartSizeEqualFirst
                ? folly::makeFuture()
                : close(firstPartAttr.value().uuid(),
                      fmt::format("{}-{}", requestId, 1));

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3), std::move(arg4),
                std::move(arg5), std::move(arg6), std::move(arg7));
        })
        //
        // List the temporary upload files
        //
        .thenValue([this, uploadId](auto &&args) {
            constexpr auto kMaxTemporaryUploadFileChildren = 10000;
            auto &bucketAttr = std::get<0>(args);
            auto &firstPartAttr = std::get<1>(args);
            auto &lastPartAttr = std::get<2>(args);
            auto &temporaryDirAttr = std::get<3>(args);
            auto &multipartETagMd5 = std::get<6>(args);

            auto arg0 = folly::makeFuture(bucketAttr);
            auto arg1 = folly::makeFuture(firstPartAttr);
            auto arg2 = folly::makeFuture(lastPartAttr);
            auto arg3 = communicate<one::messages::fuse::FileChildrenAttrs>(
                one::messages::fuse::GetFileChildrenAttrs{
                    temporaryDirAttr.value().uuid(), 0,
                    kMaxTemporaryUploadFileChildren},
                m_providerTimeout);
            auto arg4 = folly::makeFuture(multipartETagMd5);
            auto arg5 = communicate<one::messages::fuse::XAttr>(
                one::messages::fuse::GetXAttr{temporaryDirAttr.value().uuid(),
                    ONEDATA_S3_XATTR_CONTENT_TYPE},
                m_providerTimeout);
            auto arg6 = folly::makeFuture(std::move(temporaryDirAttr));

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3), std::move(arg4),
                std::move(arg5), std::move(arg6));
        })
        //
        // Remove the temporary upload files
        //
        .thenValue([this, uploadId](auto &&args) {
            auto &targetAttr = std::get<1>(args);
            auto &attrs = std::get<3>(args);
            auto &multipartETagMd5 = std::get<4>(args);
            auto &contentType = std::get<5>(args).value().value();
            auto &temporaryDirAttr = std::get<6>(args);

            std::vector<folly::Future<one::messages::fuse::FuseResponse>> futs;

            futs.emplace_back(communicate<messages::fuse::FuseResponse>(
                messages::fuse::SetXAttr{targetAttr.value().uuid(),
                    ONEDATA_S3_XATTR_CONTENT_TYPE, contentType, false, false},
                m_providerTimeout));

            for (const auto &attr : attrs.value().childrenAttrs()) {
                futs.emplace_back(communicate(
                    messages::fuse::DeleteFile{attr.uuid().toStdString()},
                    m_providerTimeout));
            }

            auto arg0 = folly::collectAll(std::move(futs));
            auto arg1 = folly::makeFuture(std::move(temporaryDirAttr));
            auto arg2 = folly::makeFuture(std::move(multipartETagMd5));
            auto arg3 = folly::makeFuture(std::move(contentType));

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3));
        })
        //
        // Delete the multipart upload temporary directory
        //
        .thenValue([this, uploadId](auto &&args) {
            auto &temporaryDirAttr = std::get<1>(args);
            auto &multipartETagMd5 = std::get<2>(args);
            auto &contentType = std::get<3>(args);

            auto arg0 = communicate(
                messages::fuse::DeleteFile{
                    temporaryDirAttr.value().uuid().toStdString()},
                m_providerTimeout);
            auto arg1 = folly::makeFuture(std::move(multipartETagMd5));
            auto arg2 = folly::makeFuture(std::move(contentType));

            return folly::collectAll(
                std::move(arg0), std::move(arg1), std::move(arg2));
        })
        //
        // Complete the multipart upload
        //
        .thenValue([this, uploadId](auto &&args) {
            auto &multipartETagMd5 = std::get<1>(args);
            auto &contentType = std::get<2>(args);

            auto arg0 = communicate(
                one::messages::fuse::CompleteMultipartUpload{
                    uploadId.toStdString()},
                m_providerTimeout);
            auto arg1 = folly::makeFuture(std::move(multipartETagMd5));
            auto arg2 = folly::makeFuture(std::move(contentType));

            return folly::collectAll(std::move(arg0), std::move(arg1));
        })
        //
        // Generate S3 CompleteMultipartUploadResult response
        //
        .thenValue([path, bucket](auto &&args) {
            auto &multipartETagMd5 = std::get<1>(args);

            Aws::S3::Model::CompleteMultipartUploadResult result;
            result.SetKey(path.toStdString());
            result.SetETag(fmt::format("\"{}\"", multipartETagMd5.value()));
            result.SetBucket(bucket.toStdString());

            return result;
        });
}

folly::Future<Aws::S3::Model::ListPartsResult>
S3Logic::listMultipartUploadParts(const folly::fbstring &uploadId,
    const folly::fbstring &bucket, const folly::fbstring &path, size_t maxParts,
    folly::Optional<size_t> partMarker)
{
    if (!partMarker.has_value())
        partMarker = 0;

    return communicate<one::messages::fuse::MultipartParts>(
        one::messages::fuse::ListMultipartParts{
            uploadId.toStdString(), maxParts, partMarker},
        m_providerTimeout)
        .thenTry([=](folly::Try<one::messages::fuse::MultipartParts> &&parts) {
            Aws::S3::Model::ListPartsResult result;
            result.SetBucket(bucket.toStdString());
            result.SetIsTruncated(!parts.value().isLast());
            result.SetKey(path.toStdString());
            result.SetMaxParts(maxParts);
            if (parts.value().nextPartMarker())
                result.SetNextPartNumberMarker(
                    parts.value().nextPartMarker().value());

            result.SetStorageClass(Aws::S3::Model::StorageClass::STANDARD);
            result.SetUploadId(uploadId.toStdString());

            for (const auto &part : parts.value().parts()) {
                Aws::S3::Model::Part resultPart;
                resultPart.SetETag(fmt::format("\"{}\"", part.etag()));
                resultPart.SetSize(part.size());
                resultPart.SetLastModified(
                    static_cast<time_t>(part.lastModified()));
                resultPart.SetPartNumber(part.partNumber());
                result.AddParts(std::move(resultPart));
            }

            return result;
        })
        .thenError(folly::tag_t<std::system_error>{},
            [bucket, path, uploadId](
                auto && /*e*/) -> Aws::S3::Model::ListPartsResult {
                throw one::s3::error::NoSuchUpload{bucket.toStdString(),
                    path.toStdString(), uploadId.toStdString()};
            });
}

folly::Future<Aws::S3::Model::ListMultipartUploadsResult>
S3Logic::listMultipartUploads(const folly::fbstring &bucket, size_t maxUploads,
    const folly::Optional<folly::fbstring> &indexToken)
{
    return getBucketAttr(bucket)
        .thenValue([this, bucket, maxUploads, indexToken](auto &&attr) {
            return communicate<one::messages::fuse::MultipartUploads>(
                one::messages::fuse::ListMultipartUploads{
                    attr.uuid(), maxUploads, indexToken},
                m_providerTimeout);
        })
        .thenValue([bucket](one::messages::fuse::MultipartUploads &&msg) {
            Aws::S3::Model::ListMultipartUploadsResult result;
            result.SetBucket(bucket.toStdString());
            result.SetIsTruncated(!msg.isLast());
            result.SetPrefix("");
            result.SetCommonPrefixes({});
            result.AddUploads({});
            result.SetNextKeyMarker("");

            return result;
        });
}

folly::Future<std::pair<Aws::S3::Model::HeadObjectResult,
    std::function<std::size_t(char *, std::size_t)>>>
S3Logic::getObject(const folly::fbstring &bucket, const folly::fbstring &path,
    const std::string &requestId,
    const folly::Optional<folly::fbstring> &rangeHeader,
    std::function<void(size_t)> completionCallback)
{
    return getBucketAttr(bucket)
        .via(m_executor.get())
        //
        // Get the attribute of the file to download
        //
        .thenTry([this, bucket, path, requestId](auto &&bucketAttr) {
            if (bucketAttr.hasException()) {
                throw one::s3::error::NoSuchBucket(
                    bucket.toStdString(), path.toStdString(), requestId);
            }
            auto arg0 = folly::makeFuture(bucketAttr);
            auto arg1 = getFileAttrByPath(bucketAttr.value().uuid(), path);

            return folly::collectAll(std::move(arg0), std::move(arg1));
        })
        //
        // Parse the range header and calculate the offset and size
        // for the request
        //
        .thenValue([bucket, path, rangeHeader, requestId](auto &&args) {
            auto bucketAttr = std::get<0>(args).value();
            auto attr = std::get<1>(args);
            if (attr.hasException()) {
                throw one::s3::error::NoSuchKey(
                    bucket.toStdString(), path.toStdString(), requestId);
            }

            if (attr.value().type() ==
                one::messages::fuse::FileAttr::FileType::directory) {
                throw one::s3::error::NoSuchKey(
                    bucket.toStdString(), path.toStdString(), requestId);
            }

            size_t requestOffset{0};
            auto requestSize{attr.value().size().value()};
            if (rangeHeader) {
                std::vector<drogon::FileRange> ranges;
                auto rangeResult = drogon::parseRangeHeader(
                    rangeHeader.value().toStdString(), requestSize, ranges);
                // only support one range for now
                if (rangeResult == drogon::SinglePart && !ranges.empty()) {
                    requestOffset = ranges[0].start;
                    requestSize = ranges[0].end - requestOffset;
                }
                else {
                    throw std::runtime_error("Unsupported range: " +
                        rangeHeader.value().toStdString());
                }
            }

            LOG_DBG(3) << "[" << requestId
                       << "] Requested offset: " << requestOffset
                       << ", request size: " << requestSize;

            auto arg0 = folly::makeFuture(std::move(bucketAttr));
            auto arg1 = folly::makeFuture(std::move(attr));
            auto arg2 = folly::makeFuture(requestOffset);
            auto arg3 = folly::makeFuture(requestSize);

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3));
        })
        //
        // Create a stream reader callback for the request
        //
        .thenValue([this, bucket, path, requestId,
                       completionCallback = std::move(completionCallback)](
                       auto &&args) mutable {
            auto bucketAttr = std::get<0>(args).value();
            auto attr = std::get<1>(args).value();
            auto requestOffset = std::get<2>(args).value();
            auto requestSize = std::get<3>(args).value();
            const auto spaceId = bucketAttr.uuid();

            std::function<std::size_t(char *, std::size_t)> streamReader =
                [this, attr, spaceId, bucket, requestId, path, requestOffset,
                    requestSize,
                    completionCallback = std::move(completionCallback)](
                    char *data, std::size_t size) mutable {
                    if (data == nullptr) {
                        // Close the file on end of stream
                        return close(attr.uuid(), requestId)
                            .via(m_executor.get())
                            .thenValue([path, requestSize,
                                           completionCallback =
                                               std::move(completionCallback)](
                                           auto && /*unit*/) -> size_t {
                                if (completionCallback)
                                    completionCallback(requestSize);
                                return 0;
                            })
                            .get();
                    }

                    return open(requestId, spaceId, attr, 0UL, O_RDONLY)
                        .via(m_executor.get())
                        .thenValue([this, requestId, size, requestOffset,
                                       requestSize](std::shared_ptr<
                                       one::client::fslogic::FuseFileHandle>
                                           &&fileHandle) mutable {
                            auto arg0 = read(std::move(fileHandle), requestId,
                                size, requestOffset, requestSize);

                            return folly::collectAll(std::move(arg0));
                        })
                        .thenValue([this, data, requestOffset, size,
                                       requestSize,
                                       requestId](auto &&args) mutable {
                            folly::IOBufQueue &buf = std::get<0>(args).value();
                            if (buf.chainLength() == 0)
                                return 0UL;

                            auto iobuf = buf.empty() ? folly::IOBuf::create(0)
                                                     : buf.move();
                            if (iobuf->isChained()) {
                                iobuf->unshare();
                                iobuf->coalesce();
                            }

                            auto bufSize = iobuf->length();

                            LOG_DBG(3)
                                << "[" << requestId << "] Read " << bufSize
                                << "/" << size << "/" << requestSize
                                << " at offset " << requestOffset;

                            memcpy(data, iobuf->data(), bufSize);

                            m_downloadedBytes.fetch_add(bufSize);

                            return bufSize;
                        })
                        .get();
                };

            auto arg0 = folly::makeFuture(std::move(bucketAttr));
            auto arg1 = folly::makeFuture(std::move(attr));
            auto arg2 = folly::makeFuture(std::move(streamReader));
            auto arg3 = folly::makeFuture(std::move(requestSize));

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3));
        })
        .thenValue([this](auto &&args) {
            auto bucketAttr = std::get<0>(args);
            auto attr = std::get<1>(args).value();
            auto streamReader = std::get<2>(args);
            auto requestSize = std::get<3>(args);

            auto arg0 = folly::makeFuture(std::move(bucketAttr));
            auto arg1 = folly::makeFuture(std::move(attr));
            auto arg2 = folly::makeFuture(std::move(streamReader));

            auto arg3 =
                communicate<messages::fuse::XAttr>(
                    messages::fuse::GetXAttr{
                        attr.uuid(), ONEDATA_S3_XATTR_CONTENT_MD5},
                    m_providerTimeout)
                    .thenTry([](folly::Try<messages::fuse::XAttr> &&xattr) {
                        if (xattr.hasException())
                            return std::string{"\"\""};

                        return xattr.value().value();
                    });

            auto arg4 =
                communicate<messages::fuse::XAttr>(
                    messages::fuse::GetXAttr{
                        attr.uuid(), ONEDATA_S3_XATTR_CONTENT_TYPE},
                    m_providerTimeout)
                    .thenTry([](folly::Try<messages::fuse::XAttr> &&xattr) {
                        if (xattr.hasException())
                            return std::string{"\"application/octet-stream\""};

                        return xattr.value().value();
                    });

            auto arg5 = folly::makeFuture(requestSize);

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3), std::move(arg4),
                std::move(arg5));
        })
        .thenValue([](auto &&args) {
            auto bucketAttr = std::get<0>(args);
            auto attr = std::get<1>(args).value();
            auto streamReader = std::get<2>(args);
            auto requestSize = std::get<5>(args).value();

            Aws::S3::Model::HeadObjectResult result;

            if (std::get<3>(args).hasValue()) {
                auto md5 = std::get<3>(args).value();
                std::string md5Value;
                one::client::util::xattr::decodeJsonXAttrValue(md5, md5Value);
                result.SetETag(md5);
            }

            if (std::get<4>(args).hasValue()) {
                auto contentType = std::get<4>(args).value();
                std::string contentTypeValue;
                one::client::util::xattr::decodeJsonXAttrValue(
                    contentType, contentTypeValue);
                result.SetContentType(contentType);
            }

            result.SetContentLength(requestSize);
            result.SetLastModified(attr.mtime());

            return folly::makeFuture(std::make_pair(
                std::move(result), std::move(streamReader.value())));
        });
}

folly::Future<Aws::S3::Model::ListObjectsV2Result> S3Logic::readDirV2Recursive(
    const folly::fbstring &bucket, const folly::fbstring &prefix,
    const folly::Optional<folly::fbstring> &marker, const size_t maxKeys,
    const bool includeDirectories)
{
    using one::messages::fuse::FileAttr;
    using one::messages::fuse::FileChildrenAttrs;
    using one::messages::fuse::FileList;
    using one::messages::fuse::GetFileChildrenAttrs;
    using one::messages::fuse::ListFilesRecursively;

    return getBucketAttr(bucket)
        .thenValue([this, maxKeys, marker, includeDirectories, prefix](
                       auto &&attr) {
            return communicate<FileList>(
                ListFilesRecursively{attr.uuid(), maxKeys, marker, {}, prefix,
                    {ONEDATA_S3_XATTR_CONTENT_MD5}, includeDirectories},
                m_providerTimeout);
        })
        .thenValue([prefix, bucket, marker, maxKeys](FileList &&attrs) {
            Aws::S3::Model::ListObjectsV2Result result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);

            int keyCount{0};

            bool lastItemWasDirectory{false};
            Aws::S3::Model::Object dirObject;
            dirObject.SetSize(0);

            for (const auto &attr : attrs.files()) {
                if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX) == 0)
                    continue;
                if (attr.name() == ".")
                    continue;

                if (attr.type() ==
                    one::messages::fuse::FileAttr::FileType::directory) {
                    // Filter out non-empty directories from the list
                    if (lastItemWasDirectory) {
                        //  The previous directory is empty it - add it now
                        result.AddContents(dirObject);
                        keyCount++;
                    }

                    dirObject.SetKey(attr.name().toStdString() + "/");
                    dirObject.SetLastModified(attr.mtime());
                    dirObject.SetETag(fmt::format("\"{}\"",
                        one::client::util::md5::md5(
                            attr.uuid().toStdString())));

                    lastItemWasDirectory = true;
                }
                else {
                    Aws::S3::Model::Object object;
                    object.SetKey(attr.name().toStdString());
                    object.SetLastModified(attr.mtime());
                    if (attr.has_xattr(ONEDATA_S3_XATTR_CONTENT_MD5))
                        object.SetETag(
                            attr.xattr(ONEDATA_S3_XATTR_CONTENT_MD5));
                    object.SetSize(*attr.size());

                    // Filter out non-empty directories from the list
                    if (lastItemWasDirectory) {
                        if (object.GetKey().find(dirObject.GetKey()) != 0) {
                            // The previous directory is empty it - add it now
                            result.AddContents(dirObject);
                        }
                    }

                    result.AddContents(std::move(object));

                    lastItemWasDirectory = false;

                    keyCount++;
                }
            }

            result.SetKeyCount(keyCount);

            bool isTruncated{true};
            if (attrs.isLast().has_value())
                isTruncated = !attrs.isLast().value();
            result.SetIsTruncated(isTruncated);
            if (!isTruncated && lastItemWasDirectory) {
                result.AddContents(std::move(dirObject));
            }

            if (marker.has_value())
                result.SetContinuationToken(marker.value().toStdString());

            if (attrs.nextPageToken())
                result.SetNextContinuationToken(
                    attrs.nextPageToken().value().toStdString());

            return result;
        });
}

folly::Future<Aws::S3::Model::ListObjectsResult> S3Logic::readDirRecursive(
    const folly::fbstring &bucket, const folly::fbstring &prefix,
    const folly::Optional<folly::fbstring> &marker, const size_t maxKeys)
{
    using one::messages::fuse::FileAttr;
    using one::messages::fuse::FileChildrenAttrs;
    using one::messages::fuse::FileList;
    using one::messages::fuse::GetFileChildrenAttrs;
    using one::messages::fuse::ListFilesRecursively;

    return getBucketAttr(bucket)
        .thenValue([this, maxKeys, marker, prefix](auto &&attr) {
            return communicate<FileList>(
                ListFilesRecursively{attr.uuid(), maxKeys, marker, {}, prefix,
                    {ONEDATA_S3_XATTR_CONTENT_MD5}},
                m_providerTimeout);
        })
        .thenValue([prefix, bucket, marker, maxKeys](FileList &&attrs) {
            Aws::S3::Model::ListObjectsResult result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);

            bool lastItemWasDirectory{false};
            Aws::S3::Model::Object dirObject;
            dirObject.SetSize(0);

            for (const auto &attr : attrs.files()) {
                if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX) == 0)
                    continue;
                if (attr.name() == ".")
                    continue;

                if (attr.type() ==
                    one::messages::fuse::FileAttr::FileType::directory) {
                    // Filter out non-empty directories from the list
                    if (lastItemWasDirectory) {
                        //  The previous directory is empty it - add it now
                        result.AddContents(dirObject);
                    }

                    dirObject.SetKey(attr.name().toStdString() + "/");
                    dirObject.SetLastModified(attr.mtime());
                    dirObject.SetETag(fmt::format("\"{}\"",
                        one::client::util::md5::md5(
                            attr.uuid().toStdString())));

                    lastItemWasDirectory = true;
                }
                else {
                    Aws::S3::Model::Object object;
                    object.SetKey(attr.name().toStdString());
                    object.SetLastModified(attr.mtime());
                    if (attr.has_xattr(ONEDATA_S3_XATTR_CONTENT_MD5))
                        object.SetETag(
                            attr.xattr(ONEDATA_S3_XATTR_CONTENT_MD5));
                    object.SetSize(*attr.size());

                    // Filter out non-empty directories from the list
                    if (lastItemWasDirectory) {
                        if (object.GetKey().find(dirObject.GetKey()) != 0) {
                            // The previous directory is empty it - add it now
                            result.AddContents(dirObject);
                        }
                    }

                    result.AddContents(std::move(object));

                    lastItemWasDirectory = false;
                }
            }

            bool isTruncated{true};
            if (attrs.isLast().has_value())
                isTruncated = !attrs.isLast().value();

            result.SetIsTruncated(isTruncated);

            if (!isTruncated && lastItemWasDirectory) {
                result.AddContents(std::move(dirObject));
            }

            if (marker.has_value())
                result.SetMarker(marker.value().toStdString());

            if (attrs.nextPageToken())
                result.SetNextMarker(
                    attrs.nextPageToken().value().toStdString());

            return result;
        });
}

folly::Future<Aws::S3::Model::ListObjectsResult> S3Logic::readDir(
    const folly::fbstring &bucket, const folly::fbstring &prefix,
    const folly::Optional<folly::fbstring> &marker,
    const folly::fbstring & /*delimiter*/, const size_t maxKeys)
{
    using one::messages::fuse::FileAttr;
    using one::messages::fuse::FileChildrenAttrs;
    using one::messages::fuse::GetFileChildrenAttrs;

    return getBucketAttr(bucket)
        .thenValue([this, prefix](FileAttr &&attr) {
            if (prefix.empty())
                return folly::makeFuture<FileAttr>(std::move(attr));

            return getFileAttr(attr.uuid(), prefix);
        })
        .thenTry([this, maxKeys, marker](auto &&attr) {
            if (attr.hasException()) {
                return folly::collectAll(folly::makeFuture(FileChildrenAttrs{}),
                    folly::makeFuture(true));
            }

            if (attr.value().type() ==
                one::messages::fuse::FileAttr::FileType::directory)
                return folly::collectAll(
                    communicate<FileChildrenAttrs>(
                        GetFileChildrenAttrs{attr.value().uuid(), 0, maxKeys,
                            marker, false, false,
                            {ONEDATA_S3_XATTR_CONTENT_MD5}},
                        m_providerTimeout),
                    folly::makeFuture(false));

            return folly::collectAll(
                folly::makeFuture(FileChildrenAttrs{std::move(attr.value())}),
                folly::makeFuture(true));
        })
        .thenTry([prefix, bucket, marker, maxKeys](auto &&args) {
            const auto &attrs = std::get<0>(args.value()).value();
            const bool isPrefixARegularFilePath =
                std::get<1>(args.value()).value();

            Aws::S3::Model::ListObjectsResult result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);

            int keyCount{0};
            for (const auto &attr : attrs.childrenAttrs()) {
                folly::fbstring prefixPrefix{prefix};
                if (!prefixPrefix.empty() && !isPrefixARegularFilePath &&
                    prefixPrefix.back() != '/')
                    prefixPrefix += "/";

                if (attr.type() ==
                    one::messages::fuse::FileAttr::FileType::directory) {
                    if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX) == 0)
                        continue;

                    Aws::S3::Model::CommonPrefix cp;
                    cp.SetPrefix(prefixPrefix.toStdString() +
                        attr.name().toStdString() + "/");
                    result.AddCommonPrefixes(std::move(cp));
                }
                else {
                    Aws::S3::Model::Object object;
                    if (!isPrefixARegularFilePath)
                        object.SetKey(prefixPrefix.toStdString() +
                            attr.name().toStdString());
                    else
                        object.SetKey(prefixPrefix.toStdString());
                    object.SetLastModified(attr.mtime());
                    if (attr.has_xattr(ONEDATA_S3_XATTR_CONTENT_MD5)) {
                        object.SetETag(
                            attr.xattr(ONEDATA_S3_XATTR_CONTENT_MD5));
                    }
                    object.SetSize(*attr.size());
                    result.AddContents(std::move(object));
                    keyCount++;
                }
            }

            bool isTruncated{true};
            if (attrs.isLast().has_value())
                isTruncated = !attrs.isLast().value();

            result.SetIsTruncated(isTruncated);

            if (marker.has_value())
                result.SetMarker(marker.value().toStdString());

            if (isTruncated && attrs.indexToken()) {
                result.SetNextMarker(attrs.indexToken().value().toStdString());
            }

            return result;
        });
}

folly::Future<Aws::S3::Model::ListObjectsV2Result> S3Logic::readDirV2(
    const folly::fbstring &bucket, const folly::fbstring &prefix,
    const folly::Optional<folly::fbstring> &marker,
    const folly::fbstring & /*delimiter*/, const size_t maxKeys)
{
    using one::messages::fuse::FileAttr;
    using one::messages::fuse::FileChildrenAttrs;
    using one::messages::fuse::GetFileChildrenAttrs;

    return getBucketAttr(bucket)
        .thenValue([this, prefix](messages::fuse::FileAttr &&attr) {
            if (prefix.empty())
                return folly::makeFuture<FileAttr>(std::move(attr));

            return getFileAttr(attr.uuid(), prefix);
        })
        .thenTry([this, maxKeys, marker](auto &&attr) {
            if (attr.hasException()) {
                return folly::collectAll(folly::makeFuture(FileChildrenAttrs{}),
                    folly::makeFuture(true));
            }

            if (attr.value().type() ==
                one::messages::fuse::FileAttr::FileType::directory)
                return folly::collectAll(
                    communicate<FileChildrenAttrs>(
                        GetFileChildrenAttrs{attr.value().uuid(), 0, maxKeys,
                            marker, false, false},
                        m_providerTimeout),
                    folly::makeFuture(false));

            return folly::collectAll(
                folly::makeFuture(FileChildrenAttrs{std::move(attr.value())}),
                folly::makeFuture(true));
        })
        .thenTry([prefix, bucket, marker, maxKeys](auto &&args) {
            const auto &attrs = std::get<0>(args.value()).value();
            const bool isPrefixARegularFilePath =
                std::get<1>(args.value()).value();

            Aws::S3::Model::ListObjectsV2Result result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);

            int keyCount{0};
            for (const auto &attr : attrs.childrenAttrs()) {
                folly::fbstring prefixPrefix{prefix};
                if (!prefixPrefix.empty() && !isPrefixARegularFilePath &&
                    prefixPrefix.back() != '/')
                    prefixPrefix += "/";

                if (attr.type() ==
                    one::messages::fuse::FileAttr::FileType::directory) {
                    if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX) == 0)
                        continue;

                    Aws::S3::Model::CommonPrefix cp;
                    cp.SetPrefix(prefixPrefix.toStdString() +
                        attr.name().toStdString() + "/");
                    result.AddCommonPrefixes(std::move(cp));
                }
                else {
                    Aws::S3::Model::Object object;
                    if (!isPrefixARegularFilePath)
                        object.SetKey(prefixPrefix.toStdString() +
                            attr.name().toStdString());
                    else
                        object.SetKey(prefixPrefix.toStdString());
                    object.SetLastModified(attr.mtime());
                    if (attr.has_xattr(ONEDATA_S3_XATTR_CONTENT_MD5)) {
                        object.SetETag(
                            attr.xattr(ONEDATA_S3_XATTR_CONTENT_MD5));
                    }
                    object.SetSize(*attr.size());
                    result.AddContents(std::move(object));
                    keyCount++;
                }
            }

            result.SetKeyCount(keyCount);

            bool isTruncated{true};
            if (attrs.isLast().has_value())
                isTruncated = !attrs.isLast().value();
            result.SetIsTruncated(isTruncated);

            if (marker.has_value()) {
                result.SetContinuationToken(marker.value().toStdString());
            }

            if (isTruncated && attrs.indexToken()) {
                result.SetNextContinuationToken(
                    attrs.indexToken().value().toStdString());
            }

            return result;
        });
}

folly::Future<Aws::S3::Model::HeadObjectResult> S3Logic::headBucket(
    const folly::fbstring &bucket, const folly::fbstring & /*requestId*/)
{
    return getBucketAttr(bucket).thenValue([](auto &&attr) {
        Aws::S3::Model::HeadObjectResult result;
        result.SetETag(one::client::util::md5::md5(attr.uuid().toStdString()));
        result.SetContentLength(0);
        result.SetContentType("application/xml");
        result.SetLastModified(attr.mtime());
        return result;
    });
}

folly::Future<Aws::S3::Model::HeadObjectResult> S3Logic::headObject(
    const folly::fbstring &bucket, const folly::fbstring &path,
    const std::string &requestId)
{
    if (path.empty()) {
        return headBucket(bucket, requestId);
    }

    return getBucketAttr(bucket)
        .thenValue([this, path](auto &&attr) {
            return getFileAttrByPath(attr.uuid(), path);
        })
        .thenTry([this, bucket, path, requestId](
                     folly::Try<messages::fuse::FileAttr> &&attr) {
            if (attr.hasException())
                throw one::s3::error::NoSuchKey(
                    bucket.toStdString(), path.toStdString(), requestId);

            const bool isDirectory = attr.value().type() ==
                one::messages::fuse::FileAttr::FileType::directory;
            if (isDirectory && path.back() != '/') {
                throw one::s3::error::NoSuchKey(
                    bucket.toStdString(), path.toStdString(), requestId);
            }

            auto uuid = attr.value().uuid();

            auto arg0 = folly::makeFuture(std::move(attr.value()));

            if (!isDirectory) {
                auto arg1 = communicate<messages::fuse::XAttr>(
                    messages::fuse::GetXAttr{
                        uuid, ONEDATA_S3_XATTR_CONTENT_MD5},
                    m_providerTimeout);
                auto arg2 = communicate<messages::fuse::XAttr>(
                    messages::fuse::GetXAttr{
                        uuid, ONEDATA_S3_XATTR_CONTENT_TYPE},
                    m_providerTimeout);

                return folly::collectAll(
                    std::move(arg0), std::move(arg1), std::move(arg2));
            }

            auto arg1 = folly::makeFuture(
                messages::fuse::XAttr{ONEDATA_S3_XATTR_CONTENT_MD5,
                    fmt::format("\"{}\"",
                        one::client::util::md5::md5(uuid.toStdString()))});
            auto arg2 = folly::makeFuture(messages::fuse::XAttr{
                ONEDATA_S3_XATTR_CONTENT_TYPE, "application/octect-stream"});

            return folly::collectAll(
                std::move(arg0), std::move(arg1), std::move(arg2));
        })
        .thenValue([](auto &&args) {
            auto attr = std::get<0>(args).value();

            Aws::S3::Model::HeadObjectResult result;
            result.SetContentLength(attr.size().value());
            result.SetLastModified(attr.mtime());

            if (!std::get<1>(args).hasException())
                result.SetETag(std::get<1>(args).value().value());

            if (!std::get<2>(args).hasException()) {
                std::string contentTypeValue;
                one::client::util::xattr::decodeJsonXAttrValue(
                    std::get<2>(args).value().value(), contentTypeValue);
                result.SetContentType(std::move(contentTypeValue));
            }

            return result;
        });
}

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
        .thenTry(
            [this, fileHandleId = fileHandle->providerHandleId()->toStdString(),
                uuid, requestId](auto && /*unit*/) {
                return communicate(
                    one::messages::fuse::FSync{
                        uuid.toStdString(), false, fileHandleId},
                    m_providerTimeout);
            })
        .thenTry(
            [this, fileHandleId = fileHandle->providerHandleId()->toStdString(),
                uuid, requestId](auto && /*unit*/) {
                LOG_DBG(3) << "Closing file for request " << requestId;
                return communicate(
                    one::messages::fuse::Release{
                        uuid.toStdString(), fileHandleId},
                    m_providerTimeout);
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

S3RequestContext &S3Logic::getRequestContext(const std::string &requestId)
{
    std::lock_guard<std::mutex> lockGuard{m_handleMutex};
    return m_requestContext.at(requestId);
}

folly::Future<std::size_t> S3Logic::write(
    std::shared_ptr<one::client::fslogic::FuseFileHandle> fileHandle,
    folly::fbstring uuid, const std::string &requestId,
    std::shared_ptr<folly::IOBuf> buf, const size_t baseOffset)
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
    auto fileBlock =
        messages::fuse::FileBlock{location.storageId(), location.fileId()};

    auto helperHandle = fileHandle->getHelperHandle(
        attr.uuid(), spaceId, location.storageId(), location.fileId());

    folly::IOBufQueue bufq{folly::IOBufQueue::cacheChainLength()};
    bufq.append(buf->clone());

    LOG_DBG(3) << "Writing to helper with timeout [ms]: "
               << std::chrono::duration_cast<std::chrono::milliseconds>(
                      m_providerTimeout)
                      .count();

    LOG_DBG(3) << "Writing " << bufq.chainLength() << "bytes at offset "
               << offset;

    return helperHandle->write(offset, std::move(bufq), {})
        .thenTry([this, uuid = std::move(uuid), offset](auto &&written) {
            return communicate(
                messages::fuse::ReportFileWritten{
                    uuid.toStdString(), offset, written.value()},
                m_providerTimeout)
                .thenTry([size = written.value()](
                             auto && /*unit*/) { return size; });
        });
}

folly::Future<folly::IOBufQueue> S3Logic::read(
    std::shared_ptr<one::client::fslogic::FuseFileHandle> fileHandle,
    const folly::fbstring &spaceId, const one::messages::fuse::FileAttr &attr,
    const std::size_t offset, const std::size_t size)
{
    return communicate<one::messages::fuse::FileLocation>(
        one::messages::fuse::GetFileLocation{attr.uuid().toStdString()},
        m_providerTimeout)
        .thenTry([attr, offset, size, spaceId, fileHandle](
                     auto &&location) -> folly::Future<folly::IOBufQueue> {
            const auto fileSize = *attr.size();
            const auto possibleRange =
                boost::icl::discrete_interval<off_t>::right_open(0, fileSize);

            const auto requestedRange =
                boost::icl::discrete_interval<off_t>::right_open(
                    offset, offset + size);

            auto wantedRange = requestedRange & possibleRange;

            assert(boost::icl::size(wantedRange) > 0);

            if (boost::icl::size(wantedRange) <= 0) {
                return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
            }

            boost::icl::discrete_interval<off_t> availableRange = possibleRange;

            auto fileBlock = messages::fuse::FileBlock{
                location.value().storageId(), location.value().fileId()};

            const auto wantedAvailableRange = availableRange & wantedRange;

            if (boost::icl::size(wantedAvailableRange) <= 0) {
                return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
            }

            const std::size_t availableSize =
                boost::icl::size(wantedAvailableRange);
            const std::size_t continuousSize =
                boost::icl::size(boost::icl::left_subtract(availableRange,
                    boost::icl::discrete_interval<off_t>::right_open(
                        0, offset)));

            auto helperHandle = fileHandle->getHelperHandle(attr.uuid(),
                spaceId, fileBlock.storageId(), fileBlock.fileId());

            return helperHandle->readContinuous(
                offset, availableSize, continuousSize);
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

    auto fileBlock =
        messages::fuse::FileBlock{location.storageId(), location.fileId()};

    const auto wantedAvailableRange = availableRange & wantedRange;

    if (boost::icl::size(wantedAvailableRange) <= 0) {
        return folly::IOBufQueue{folly::IOBufQueue::cacheChainLength()};
    }

    const std::size_t wantedAvailableSize =
        boost::icl::size(wantedAvailableRange);
    const std::size_t continuousSize = wantedAvailableSize;

    auto helperHandle = fileHandle->getHelperHandle(
        attr.uuid(), spaceId, fileBlock.storageId(), fileBlock.fileId());

    assert(offset + requestOffset + wantedAvailableSize <= fileSize);
    assert(wantedAvailableSize <= size);

    return helperHandle
        ->readContinuous(
            offset + requestOffset, wantedAvailableSize, continuousSize)
        .thenValue([this, requestId, size, offset, requestOffset, requestSize](
                       folly::IOBufQueue &&buf) {
            LOG_DBG(3) << "[" << requestId << ", " << offset + requestOffset
                       << ", " << requestSize << "] Read from helper "
                       << buf.chainLength() << "/" << size << " at offset "
                       << offset + requestOffset;

            getRequestContext(requestId).offset += buf.chainLength();
            return std::move(buf);
        });
}

folly::Future<one::messages::fuse::Uuid> S3Logic::resolveGuid(
    const folly::fbstring &path)
{
    return communicate<one::messages::fuse::Uuid>(
        one::messages::fuse::ResolveGuid{path.toStdString()},
        m_providerTimeout);
}

folly::Future<one::messages::fuse::FileAttr> S3Logic::getFileAttr(
    const folly::fbstring &spaceId, const folly::fbstring &path)
{
    std::vector<std::string> pathVector;
    folly::split("/", path, pathVector, true);

    std::reverse(pathVector.begin(), pathVector.end());

    return getFileAttr(spaceId, pathVector);
}

folly::Future<one::messages::fuse::FileAttr> S3Logic::getBucketAttr(
    const folly::fbstring &bucket)
{
    if (m_bucketIdCache.find(bucket) != m_bucketIdCache.end())
        return folly::makeFuture(m_bucketIdCache.at(bucket));

    return communicate<one::messages::fuse::FileAttr>(
        one::messages::fuse::GetChildAttr{m_rootUuid, bucket},
        m_providerTimeout)
        .thenTry([this, bucket](
                     folly::Try<one::messages::fuse::FileAttr> &&bucketAttr) {
            m_bucketIdCache.emplace(bucket, bucketAttr.value());
            return std::move(bucketAttr);
        });
}

folly::Future<one::messages::fuse::FileAttr> S3Logic::getFileAttr(
    const folly::fbstring &parentId, std::vector<std::string> path)
{
    assert(!path.empty());

    if (path.size() == 1) {
        return communicate<one::messages::fuse::FileAttr>(
            one::messages::fuse::GetChildAttr{parentId, path[0]},
            m_providerTimeout);
    }

    auto nextChildName = path.back();
    path.pop_back();

    return communicate<one::messages::fuse::FileAttr>(
        one::messages::fuse::GetChildAttr{parentId, nextChildName},
        m_providerTimeout)
        .thenValue([this, nextChildName, path = std::move(path)](
                       auto &&attr) { return getFileAttr(attr.uuid(), path); });
}

folly::Future<size_t> S3Logic::uploadObject(const std::string &requestId,
    const folly::fbstring &bucket, const folly::fbstring &path,
    const folly::fbstring &md5, const folly::fbstring &contentType,
    std::shared_ptr<folly::IOBuf> buf)
{
    return getBucketAttr(bucket)
        //
        // Get temporary upload directory attr or create it if necessary
        //
        .thenTry([this, path, bucket, requestId](auto &&bucketAttr) {
            if (bucketAttr.hasException()) {
                throw one::s3::error::NoSuchBucket(
                    bucket.toStdString(), path.toStdString(), requestId);
            }
            auto arg0 = folly::makeFuture(bucketAttr.value());
            auto arg1 = communicate<messages::fuse::FileAttr>(
                messages::fuse::CreatePath{
                    bucketAttr.value().uuid(), ONEDATA_S3_MULTIPART_PREFIX},
                m_providerTimeout);

            return folly::collectAll(std::move(arg0), std::move(arg1))
                .via(m_executor.get());
        })
        //
        // Create temporary file for upload
        //
        .thenValue([this, requestId](auto &&args) {
            constexpr auto kDefaultFilePerms{0655};
            auto &bucketAttr = std::get<0>(args);
            auto &tmpDirAttr = std::get<1>(args);

            folly::fbstring parentUuid = tmpDirAttr.value().uuid();

            const auto tmpFileName =
                getCompleteUploadTemporaryFileName(requestId);

            auto arg0 = folly::makeFuture(std::move(bucketAttr));
            auto arg1 = create(
                requestId, parentUuid, tmpFileName, S_IFREG, kDefaultFilePerms);

            return folly::collectAll(std::move(arg0), std::move(arg1));
        })
        //
        // Open the temporary file
        //
        .thenValue([this, requestId, bucket, path](auto &&args) {
            const auto &bucketAttr = std::get<0>(args);
            const auto &attr = std::get<1>(args);

            folly::fbstring spaceId = bucketAttr.value().uuid();

            auto arg0 = folly::makeFuture(std::move(bucketAttr));
            auto arg2 = open(requestId, spaceId, attr.value(), 0UL, O_WRONLY);
            auto arg1 = folly::makeFuture(std::move(attr));

            return folly::collectAll(
                std::move(arg0), std::move(arg1), std::move(arg2));
        })
        //
        // Write data to the temporary file and create target path if
        // necessary
        //
        .thenValue([this, requestId, path, buf = std::move(buf)](
                       auto &&args) mutable {
            const auto &bucketAttr = std::get<0>(args).value();
            const auto &attr = std::get<1>(args);
            const auto &fileHandle = std::get<2>(args).value();

            folly::fbstring spaceId = attr.value().uuid();
            auto arg0 = folly::makeFuture(std::move(attr));
            auto arg1 = getFileParentAttrByPath(bucketAttr, path);
            auto arg2 = write(fileHandle, spaceId, requestId, std::move(buf));
            auto arg3 = folly::makeFuture(fileHandle);

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3));
        })
        //
        // Release temporary file handle
        //
        .thenValue(
            [this, requestId, bucket, path, buf = std::move(buf)](auto &&args) {
                const auto &tmpAttr = std::get<0>(args).value();
                const auto &targetParentAttr = std::get<1>(args).value();
                const auto written = std::get<2>(args).value();
                const auto fileHandle = std::get<3>(args).value();

                auto arg0 = folly::makeFuture(tmpAttr);
                auto arg1 = folly::makeFuture(targetParentAttr);
                auto arg2 = folly::makeFuture(written);
                auto arg3 = close(tmpAttr.uuid(), requestId);

                return folly::collectAll(std::move(arg0), std::move(arg1),
                    std::move(arg2), std::move(arg3));
            })
        //
        // Set the md5 checksum and content type as xattr on the file
        //
        .thenValue([this, md5, contentType](auto &&args) {
            const auto &tmpAttr = std::get<0>(args).value();
            const auto &targetParentAttr = std::get<1>(args).value();
            const auto written = std::get<2>(args).value();

            auto arg0 = folly::makeFuture(tmpAttr);
            auto arg1 = folly::makeFuture(targetParentAttr);
            auto arg2 = folly::makeFuture(written);
            auto arg3 = communicate<messages::fuse::FuseResponse>(
                messages::fuse::SetXAttr{tmpAttr.uuid(),
                    ONEDATA_S3_XATTR_CONTENT_MD5,
                    fmt::format("\"{}\"", md5.toStdString()), false, false},
                m_providerTimeout);
            auto arg4 = communicate<messages::fuse::FuseResponse>(
                messages::fuse::SetXAttr{tmpAttr.uuid(),
                    ONEDATA_S3_XATTR_CONTENT_TYPE,
                    fmt::format("\"{}\"", contentType.toStdString()), false,
                    false},
                m_providerTimeout);

            return folly::collectAll(std::move(arg0), std::move(arg1),
                std::move(arg2), std::move(arg3), std::move(arg4));
        })
        //
        // Rename the temporary file to the target file
        //
        .thenValue([this, requestId, bucket, path](auto &&args) {
            const auto &tmpAttr = std::get<0>(args).value();
            const auto &targetParentAttr = std::get<1>(args).value();
            const auto written = std::get<2>(args).value();

            const auto &oldUuid = tmpAttr.uuid().toStdString();
            const auto &newParentUuid = targetParentAttr.uuid().toStdString();
            const auto &newName = getFileNameFromPath(path).toStdString();

            auto arg0 = folly::makeFuture(written);
            auto arg1 = communicate<messages::fuse::FileRenamed>(
                messages::fuse::Rename{oldUuid, newParentUuid, newName},
                m_providerTimeout);

            m_uploadedBytes.fetch_add(written);

            return folly::collectAll(std::move(arg0), std::move(arg1));
        })
        .thenValue([](auto &&args) { return std::get<0>(args).value(); });
}

folly::Future<folly::Unit> S3Logic::deleteObject(
    const std::string & /*requestId*/, const folly::fbstring &bucket,
    const folly::fbstring &path)
{
    return getBucketAttr(bucket)
        .thenValue([this, path](auto &&attr) {
            return getFileAttrByPath(attr.uuid(), path);
        })
        .thenTry([this](folly::Try<one::messages::fuse::FileAttr> &&value) {
            if (value.hasException()) {
                value.throwIfFailed();
            }

            return communicate(
                messages::fuse::DeleteFile{value.value().uuid().toStdString()},
                m_providerTimeout);
        })
        .thenValue([](auto && /*msg*/) { return folly::Unit{}; });
}

folly::Future<std::shared_ptr<one::client::fslogic::FuseFileHandle>>
S3Logic::open(std::string requestId, const folly::fbstring &spaceId,
    const one::messages::fuse::FileAttr &attr, const size_t offset,
    const int flags)
{
    LOG_FCALL() << LOG_FARG(spaceId) << LOG_FARGH(flags);

    std::lock_guard<std::mutex> lockGuard{m_handleMutex};

    if (m_requestHandles.find(requestId) != m_requestHandles.end())
        return m_requestHandles.at(requestId);

    const auto filteredFlags = flags;
    const auto flag = detail::getOpenFlag(helpers::maskToFlags(filteredFlags));
    messages::fuse::OpenFile msg{attr.uuid().toStdString(), flag};

    LOG_DBG(2) << "Sending file opened message for " << attr.uuid();

    return communicate<messages::fuse::FileOpened>(
        std::move(msg), m_providerTimeout)
        .thenValue([this, filteredFlags, flags, attr, requestId](
                       auto &&opened) {
            std::lock_guard<std::mutex> lockGuard{m_handleMutex};

            m_requestHandles.emplace(requestId,
                std::make_shared<client::fslogic::FuseFileHandle>(filteredFlags,
                    opened.handleId(),
                    std::shared_ptr<
                        client::cache::OpenFileMetadataCache::OpenFileToken>{},
                    m_helpersCache, m_forceProxyIOCache, m_providerTimeout));
            m_requestFileHandleFlags.emplace(requestId, flags);

            LOG_DBG(2) << "Assigned fuse handle " << requestId << " for file "
                       << attr.uuid();

            return communicate<one::messages::fuse::FileLocation>(
                one::messages::fuse::GetFileLocation{attr.uuid().toStdString()},
                m_providerTimeout);
        })
        .thenValue([this, spaceId, offset, attr, requestId](
                       one::messages::fuse::FileLocation &&fileLocation) {
            S3RequestContext context{};
            context.spaceId = spaceId;
            context.location = fileLocation;
            context.attr = attr;
            context.offset = offset;

            std::lock_guard<std::mutex> lockGuard{m_handleMutex};
            m_requestContext.emplace(requestId, std::move(context));

            return m_requestHandles.at(requestId);
        });
}

folly::Future<one::messages::fuse::FileAttr> S3Logic::create(
    std::string requestId, const folly::fbstring &parentUuid,
    const folly::fbstring &path, const mode_t mode, const int flags)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(path) << LOG_FARG(mode)
                << LOG_FARG(flags);

    if (S_ISDIR(mode) || S_ISCHR(mode) || S_ISBLK(mode) || S_ISFIFO(mode) ||
        S_ISLNK(mode) || S_ISSOCK(mode)) {

        LOG(ERROR) << "Attempt to create unsupported file type - only regular "
                      "files are supported for this call.";
        return folly::makeFutureWith([]() -> one::messages::fuse::FileAttr {
            throw std::errc::operation_not_supported; // NOLINT
        });
    }

    return folly::makeSemiFutureWith([this, path, parentUuid]() {
        std::vector<std::string> pathTokens;
        folly::split("/", path, pathTokens, false);

        if (pathTokens.size() > 1) {
            std::vector<std::string> pathParentTokens = pathTokens;
            pathParentTokens.pop_back();
            messages::fuse::CreatePath msg{parentUuid,
                fmt::format("{}", fmt::join(pathParentTokens, "/"))};

            return folly::collectAll(communicate<messages::fuse::FileAttr>(
                                         std::move(msg), m_providerTimeout)
                                         .thenValue([](auto &&parentAttr) {
                                             return parentAttr.uuid();
                                         }),
                folly::makeFuture(pathTokens.back()));
        }

        return folly::collectAll(folly::makeFuture(parentUuid),
            folly::makeFuture(pathTokens.back()));
    })
        .via(m_executor.get())
        .thenValue([this, mode, flags](auto &&args) {
            const auto &effectiveParentUuid = std::get<0>(args).value();
            const auto &name = std::get<1>(args).value();

            constexpr auto modeMask =
                S_ISUID | S_ISGID | S_ISVTX | S_IRWXU | S_IRWXG | S_IRWXO;
            const auto flag = detail::getOpenFlag(helpers::maskToFlags(flags));

            return communicate<messages::fuse::FileCreated>(
                messages::fuse::CreateFile{
                    effectiveParentUuid, name, mode & modeMask, flag},
                m_providerTimeout);
        })
        .thenValue([this, path, flags, parentUuid, requestId](auto &&created) {
            const auto &uuid = created.attr().uuid();

            S3RequestContext context{};
            context.spaceId = parentUuid;
            context.location = created.location();
            context.attr = created.attr();
            context.offset = 0;

            auto fuseFileHandle =
                std::make_shared<client::fslogic::FuseFileHandle>(flags,
                    created.handleId(),
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
    const folly::fbstring &uuid,
    std::shared_ptr<client::fslogic::FuseFileHandle> fileHandle)
{
    folly::fbvector<folly::Future<folly::Unit>> releaseFutures;
    for (auto &helperHandle : fileHandle->helperHandles())
        releaseFutures.emplace_back(helperHandle->release());

    return folly::collectAll(std::move(releaseFutures))
        .via(m_executor.get())
        .thenValue([this, fileHandle, uuid](auto && /*futs*/) {
            return communicate(
                messages::fuse::Release{uuid.toStdString(),
                    fileHandle->providerHandleId()->toStdString()},
                m_providerTimeout);
        })
        .thenValue([](auto && /*status*/) { return folly::makeFuture(); });
}

folly::Future<messages::fuse::FileAttr> S3Logic::getFileAttrByPath(
    const folly::fbstring &bucketId, const folly::fbstring &path)
{
    return communicate<messages::fuse::FileAttr>(
        messages::fuse::GetFileAttrByPath{
            bucketId.toStdString(), path.toStdString(), {}},
        m_providerTimeout);
}

folly::Future<messages::fuse::FileAttr> S3Logic::getFileParentAttrByPath(
    const messages::fuse::FileAttr &bucketAttr, const folly::fbstring &path)
{
    auto parentAttr = bucketAttr;
    std::vector<std::string> pathTokens;
    folly::split("/", path, pathTokens, false);
    if (pathTokens.size() == 1) {
        return folly::makeFuture<messages::fuse::FileAttr>(
            std::move(parentAttr));
    }

    auto pathParentTokens = pathTokens;
    pathParentTokens.pop_back();
    auto parentPath = fmt::format("{}", fmt::join(pathParentTokens, "/"));
    return communicate<messages::fuse::FileAttr>(
        messages::fuse::CreatePath{bucketAttr.uuid(), parentPath},
        m_providerTimeout);
}

S3LogicCache::S3LogicCache(
    std::shared_ptr<one::client::options::Options> options)
    : m_options{std::move(options)}
    , m_initialized{true}
    , m_executor{std::make_shared<folly::IOThreadPoolExecutor>(
          std::thread::hardware_concurrency())}
{
}

folly::Future<std::shared_ptr<S3Logic>> S3LogicCache::get(
    const folly::fbstring &token)
{
    assert(m_options);

    folly::fbstring effectiveToken = token;
    if (effectiveToken.empty())
        effectiveToken = m_options->getAccessToken().get();

    try {
        std::lock_guard<std::mutex> lock{m_cacheMutex};

        if (m_cache.find(effectiveToken) == m_cache.end()) {
            auto p = std::make_shared<
                folly::SharedPromise<std::shared_ptr<S3Logic>>>();
            m_cache.emplace(effectiveToken, p);

            auto s3LogicPtr = std::make_shared<S3Logic>(
                m_options, effectiveToken, m_executor);

            s3LogicPtr->connect().thenTry(
                [this, effectiveToken, s3LogicPtr, p = std::move(p)](
                    auto &&s3Logic) mutable {
                    if (s3Logic.hasException()) {
                        std::lock_guard<std::mutex> lock{m_cacheMutex};
                        m_cache.erase(effectiveToken);
                        throw one::s3::error::AccessDenied("", "", "");
                    }

                    p->setValue(std::move(s3Logic.value()));
                });
        }

        return m_cache.at(effectiveToken)->getFuture();
    }
    catch (...) {
        std::lock_guard<std::mutex> lock{m_cacheMutex};
        m_cache.erase(effectiveToken);
        return folly::makeFutureWith([]() -> std::shared_ptr<S3Logic> {
            throw one::s3::error::AccessDenied("", "", "");
        });
    }
}

} // namespace s3
} // namespace one