/**
 * @file s3LogicMultipart.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Logic.h"

#include "futureUtils.h"
#include "messages/fuse/abortMultipartUpload.h"
#include "messages/fuse/completeMultipartUpload.h"
#include "messages/fuse/createMultipartUpload.h"
#include "messages/fuse/createPath.h"
#include "messages/fuse/deleteFile.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileCreated.h"
#include "messages/fuse/fileList.h"
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/getFileAttrByPath.h"
#include "messages/fuse/getFileChildrenAttrs.h"
#include "messages/fuse/getXAttr.h"
#include "messages/fuse/listMultipartParts.h"
#include "messages/fuse/listMultipartUploads.h"
#include "messages/fuse/multipartParts.h"
#include "messages/fuse/multipartUpload.h"
#include "messages/fuse/multipartUploads.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/setXAttr.h"
#include "messages/fuse/uploadMultipartPart.h"
#include "monitoring/monitoring.h"

#include <spdlog/spdlog.h>

#include <tuple>
#include <utility>

namespace one {
namespace s3 {

using one::client::fslogic::FuseFileHandle;
using one::messages::fuse::AbortMultipartUpload;
using one::messages::fuse::CompleteMultipartUpload;
using one::messages::fuse::CreateMultipartUpload;
using one::messages::fuse::CreatePath;
using one::messages::fuse::DeleteFile;
using one::messages::fuse::FileAttr;
using one::messages::fuse::FileChildrenAttrs;
using one::messages::fuse::FileRenamed;
using one::messages::fuse::FuseResponse;
using one::messages::fuse::GetFileAttrByPath;
using one::messages::fuse::GetFileChildrenAttrs;
using one::messages::fuse::GetXAttr;
using one::messages::fuse::ListMultipartParts;
using one::messages::fuse::ListMultipartUploads;
using one::messages::fuse::MultipartParts;
using one::messages::fuse::MultipartUpload;
using one::messages::fuse::MultipartUploads;
using one::messages::fuse::Rename;
using one::messages::fuse::SetXAttr;
using one::messages::fuse::UploadMultipartPart;
using one::messages::fuse::XAttr;

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
                     folly::Try<FileAttr> &&maybeBucketAttr) {
            if (maybeBucketAttr.hasException()) {
                throw one::s3::error::NoSuchBucket{bucket.toStdString(),
                    path.toStdString(), requestId.toStdString()};
            }

            auto spaceId = maybeBucketAttr.value().uuid();
            auto spaceUuid =
                one::client::util::uuid::uuidToSpaceId(spaceId).toStdString();

            auto bucketAttr = folly::makeFuture(std::move(maybeBucketAttr));
            auto upload = communicate<MultipartUpload>(CreateMultipartUpload{
                std::move(spaceUuid), path.toStdString()});

            PUSH_FUTURES_2(bucketAttr, upload);
        })
        //
        // Create temporary upload directory
        //
        .thenValue([this](auto &&args) {
            POP_FUTURES_2(args, bucketAttr, upload);

            const auto tmpDirId = one::client::util::uuid::uuidToTmpDirId(
                bucketAttr.value().uuid());

            auto uploadTmpDirAttr = communicate<FileAttr>(CreatePath{
                tmpDirId, getMultipartUploadTemporaryDir(upload.value().id())});

            PUSH_FUTURES_2(upload, uploadTmpDirAttr);
        })
        //
        // Set the content-type metadata on the directory
        //
        .thenValue([contentType, this](auto &&args) {
            POP_FUTURES_2(args, upload, uploadTmpDirAttr);

            auto setXAttr = communicate<FuseResponse>(SetXAttr{
                uploadTmpDirAttr.value().uuid(), ONEDATA_S3_XATTR_CONTENT_TYPE,
                fmt::format("\"{}\"", contentType), false, false});

            PUSH_FUTURES_2(upload, setXAttr);
        })
        //
        // Generate CreateMultipartUploadResult response
        //
        .thenValue([this, bucket, path](auto &&args) {
            POP_FUTURES_1(args, upload);

            return toCreateMultipartUploadResult(
                std::move(upload.value()), bucket, path);
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
        .thenValue([this, uploadId](FileAttr &&bucketAttr) {
            const auto temporaryUploadPath =
                getMultipartUploadTemporaryDir(uploadId);

            auto attr = communicate<FileAttr>(
                GetFileAttrByPath{bucketAttr.uuid(), temporaryUploadPath, {}});

            PUSH_FUTURES_1(attr);
        })
        //
        // List the temporary upload files
        //
        .thenValue([this](auto &&args) {
            POP_FUTURES_1(args, attr);

            auto attrs = communicate<FileChildrenAttrs>(GetFileChildrenAttrs{
                attr.value().uuid().toStdString(), 0, kMaxListSize});

            PUSH_FUTURES_1(attrs);
        })
        //
        // Remove the temporary upload files
        //
        .thenValue([this](auto &&args) {
            POP_FUTURES_1(args, attrs);

            std::vector<folly::Future<FuseResponse>> futs;
            for (const auto &attr : attrs.value().childrenAttrs()) {
                futs.emplace_back(
                    communicate(DeleteFile{attr.uuid().toStdString()}));
            }

            return futs;
        })
        //
        // Abort multipart upload
        //
        .thenValue([this, uploadId](auto && /*response*/) {
            return communicate(AbortMultipartUpload{uploadId.toStdString()});
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
                       FileAttr &&bucketAttr) {
            auto filePerms = m_options->getOneS3FileMode();

            const auto tmpDirId =
                one::client::util::uuid::uuidToTmpDirId(bucketAttr.uuid());

            const auto tmpPath = fmt::format("{}-{}",
                getMultipartUploadTemporaryFileName(path, uploadId)
                    .toStdString(),
                partSize);

            auto attr = getFileAttr(bucketAttr.uuid(), tmpPath)
                            .thenError(folly::tag_t<std::system_error>{},
                                [this, tmpPath, parentUuid = tmpDirId,
                                    requestId, path, filePerms](auto && /*e*/) {
                                    return create(requestId, parentUuid,
                                        tmpPath, S_IFREG | filePerms, O_WRONLY);
                                })
                            .thenError(folly::tag_t<std::system_error>{},
                                [this, tmpPath, parentUuid = tmpDirId,
                                    requestId, path](auto && /*e*/) {
                                    // sic
                                    return getFileAttr(parentUuid, tmpPath);
                                });

            PUSH_FUTURES_3(bucketAttr, attr, tmpPath);
        })
        //
        // Open the part specific file
        //
        .thenValue([this, bucket, path, uploadId, requestId](auto &&args) {
            POP_FUTURES_3(args, bucketAttr, attr, tmpPath);

            if (attr.hasException()) {
                LOG_DBG(1) << "Failed to create multipart upload part file "
                           << tmpPath.value();
                throw one::s3::error::InternalServerError{bucket.toStdString(),
                    path.toStdString(), uploadId.toStdString()};
            }

            auto fileHandle = open(requestId, bucketAttr.value().uuid(),
                attr.value(), 0UL, O_WRONLY);

            PUSH_FUTURES_2(attr, fileHandle);
        })
        //
        // Write the part contents to the temporary file
        //
        .thenValue([this, requestId, partSize, partNumber,
                       buf = std::move(buf)](auto &&args) mutable {
            POP_FUTURES_2(args, attr, fileHandle);

            auto written = write(fileHandle.value(), attr.value().uuid(),
                requestId, std::move(buf), (partNumber - 1) * partSize);

            PUSH_FUTURES_2(attr, written);
        })
        //
        // Close part file
        //
        .thenValue([this, requestId](auto &&args) {
            POP_FUTURES_2(args, attr, written);

            m_uploadedBytes.fetch_add(written.value());

            return close(attr.value().uuid(), requestId);
        })
        //
        // Report multipart part upload complete
        //
        .thenValue(
            [this, uploadId, partMD5, partNumber, partSize](auto && /*unit*/) {
                return communicate(UploadMultipartPart{uploadId.toStdString(),
                    partMD5.toStdString(),
                    static_cast<uint64_t>(std::time(nullptr)), partNumber,
                    partSize});
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
    auto bucketAttr = getBucketAttr(bucket);
    auto parts =
        listMultipartUploadParts(uploadId, bucket, path, kMaxPartsList, {0});

    return folly::collectAll(std::move(bucketAttr), std::move(parts))
        .via(folly::getGlobalCPUExecutor().get())
        //
        // Determine the attributes of the first part and last part
        //
        .thenValue([this, uploadId, path](auto &&args) {
            POP_FUTURES_2(args, bucketAttr, parts);

            auto spaceId = bucketAttr.value().uuid();

            const auto firstPartSize =
                parts.value().GetParts().front().GetSize();
            const auto lastPartSize = parts.value().GetParts().back().GetSize();
            const auto lastPartNumber =
                parts.value().GetParts().back().GetPartNumber();

            const auto tmpDirId = one::client::util::uuid::uuidToTmpDirId(
                bucketAttr.value().uuid());

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

            auto firstPartAttr = getFileAttrByPath(tmpDirId, firstPartPath);
            auto lastPartAttr = getFileAttrByPath(tmpDirId, lastPartPath);

            PUSH_FUTURES_7(bucketAttr, firstPartAttr, lastPartAttr,
                lastPartNumber, lastPartSize, firstPartSize, multipartETagMd5);
        })
        //
        // Open first part for writing to append remaining parts to the end
        // of it
        // TODO: Currently this code only handles cases where all parts
        //       except the last one have the same size
        //
        .thenValue([this, requestId](auto &&args) {
            POP_FUTURES_7(args, bucketAttr, firstPartAttr, lastPartAttr,
                lastPartNumber, lastPartSize, firstPartSize, multipartETagMd5);

            const bool isLastPartSizeEqualFirst =
                firstPartAttr.value().uuid() == lastPartAttr.value().uuid();

            const auto tmpDirId = one::client::util::uuid::uuidToTmpDirId(
                bucketAttr.value().uuid());

            auto tmpLastPartFileHandle = isLastPartSizeEqualFirst
                ? folly::makeFuture(std::shared_ptr<FuseFileHandle>{})
                : open(fmt::format("{}-{}", requestId, lastPartNumber.value()),
                      bucketAttr.value().uuid(), lastPartAttr.value(),
                      (lastPartNumber.value() - 1) * lastPartSize.value(),
                      O_RDONLY, lastPartSize.value());
            auto setXattrResponse = communicate<FuseResponse>(SetXAttr{
                firstPartAttr.value().uuid(), ONEDATA_S3_XATTR_CONTENT_MD5,
                fmt::format("\"{}\"", multipartETagMd5.value()), false, false});

            PUSH_FUTURES_9(bucketAttr, firstPartAttr, lastPartAttr,
                lastPartNumber, lastPartSize, firstPartSize,
                tmpLastPartFileHandle, multipartETagMd5, setXattrResponse);
        })
        //
        // Read the contents of the last part
        //
        .thenValue([this, requestId, bucket, path](auto &&args) {
            POP_FUTURES_8(args, bucketAttr, firstPartAttr, lastPartAttr,
                lastPartNumber, lastPartSize, firstPartSize,
                tmpLastPartFileHandle, multipartETagMd5);

            const auto spaceId = bucketAttr.value().uuid();
            const bool isLastPartSizeEqualFirst =
                firstPartAttr.value().uuid() == lastPartAttr.value().uuid();

            const auto tmpDirId =
                one::client::util::uuid::uuidToTmpDirId(spaceId);

            const auto lastPartOffset =
                lastPartSize.value() * (lastPartNumber.value() - 1);
            auto bufQueue = isLastPartSizeEqualFirst
                ? folly::makeFuture(folly::IOBufQueue{})
                : read(tmpLastPartFileHandle.value(),
                      fmt::format("{}-{}", requestId, lastPartNumber.value()),
                      lastPartAttr.value(), lastPartOffset,
                      lastPartSize.value());
            auto tmpTargetFileHandle = isLastPartSizeEqualFirst
                ? folly::makeFuture(
                      std::shared_ptr<one::client::fslogic::FuseFileHandle>{})
                : open(fmt::format("{}-{}", requestId, 1), tmpDirId,
                      firstPartAttr.value(), 0UL, O_RDWR | O_APPEND);
            auto arg7 = folly::makeFuture(tmpLastPartFileHandle);
            auto arg8 = folly::makeFuture(multipartETagMd5);

            PUSH_FUTURES_9(bucketAttr, firstPartAttr, lastPartAttr,
                lastPartNumber, firstPartSize, bufQueue, tmpTargetFileHandle,
                tmpLastPartFileHandle, multipartETagMd5);
        })
        //
        // Append the contents of the last part to the first part
        //
        .thenValue([this, requestId, bucket, path, uploadId](auto &&args) {
            POP_FUTURES_9(args, bucketAttr, firstPartAttr, lastPartAttr,
                lastPartNumber, firstPartSize, bufQueue, tmpTargetFileHandle,
                tmpLastPartFileHandle, multipartETagMd5);

            const bool isLastPartSizeEqualFirst =
                firstPartAttr.value().uuid() == lastPartAttr.value().uuid();

            const auto firstPartPath = fmt::format("{}-{}",
                getMultipartUploadTemporaryFileName(path, uploadId)
                    .toStdString(),
                firstPartSize.value());

            const auto tmpDirId = one::client::util::uuid::uuidToTmpDirId(
                bucketAttr.value().uuid());

            const auto appendOffset =
                firstPartSize.value() * (lastPartNumber.value() - 1);
            auto written = isLastPartSizeEqualFirst
                ? folly::makeFuture<size_t>(0)
                : write(tmpTargetFileHandle.value(),
                      firstPartAttr.value().uuid(),
                      fmt::format("{}-{}", requestId, 1),
                      std::make_shared<folly::IOBuf>(
                          bufQueue.value().moveAsValue()),
                      appendOffset);
            auto temporaryDirAttr = getFileAttrByPath(
                tmpDirId, getMultipartUploadTemporaryDir(uploadId));
            auto targetParentAttr =
                getFileParentAttrByPath(bucketAttr.value(), path);
            auto closeStatus = isLastPartSizeEqualFirst
                ? folly::makeFuture()
                : close(lastPartAttr.value().uuid(),
                      fmt::format("{}-{}", requestId, lastPartNumber.value()));

            PUSH_FUTURES_9(bucketAttr, firstPartAttr, lastPartAttr, written,
                temporaryDirAttr, targetParentAttr, closeStatus,
                tmpTargetFileHandle, multipartETagMd5);
        })
        //
        //
        // Rename the temporary complete file to target file
        //
        .thenValue([this, uploadId, path, requestId](auto &&args) {
            POP_FUTURES_9(args, bucketAttr, firstPartAttr, lastPartAttr,
                written, temporaryDirAttr, targetParentAttr, closeStatus,
                tmpTargetFileHandle, multipartETagMd5);

            const auto &firstPartUuid =
                firstPartAttr.value().uuid().toStdString();
            const auto &newParentUuid =
                targetParentAttr.value().uuid().toStdString();
            const auto &newName = getFileNameFromPath(path).toStdString();

            auto contentType =
                one::client::util::mime::mimeFromPath(path.toStdString());

            const bool isLastPartSizeEqualFirst =
                firstPartAttr.value().uuid() == lastPartAttr.value().uuid();

            auto fileRenamedStatus = communicate<FileRenamed>(
                Rename{firstPartUuid, newParentUuid, newName});
            auto setXattrStatus = communicate<FuseResponse>(
                SetXAttr{firstPartUuid, ONEDATA_S3_XATTR_CONTENT_TYPE,
                    fmt::format("\"{}\"", contentType), false, false});
            auto closeFirstPartStatus = isLastPartSizeEqualFirst
                ? folly::makeFuture()
                : close(firstPartAttr.value().uuid(),
                      fmt::format("{}-{}", requestId, 1));

            PUSH_FUTURES_8(bucketAttr, firstPartAttr, lastPartAttr,
                temporaryDirAttr, fileRenamedStatus, setXattrStatus,
                multipartETagMd5, closeFirstPartStatus);
        })
        //
        // List the temporary upload files
        //
        .thenValue([this, uploadId](auto &&args) {
            POP_FUTURES_8(args, bucketAttr, firstPartAttr, lastPartAttr,
                temporaryDirAttr, fileRenamedStatus, setXattrStatus,
                multipartETagMd5, closeFirstPartStatus);

            constexpr auto kMaxTemporaryUploadFileChildren = 10000;

            auto attrs = communicate<FileChildrenAttrs>(
                GetFileChildrenAttrs{temporaryDirAttr.value().uuid(), 0,
                    kMaxTemporaryUploadFileChildren});
            auto contentType =
                communicate<XAttr>(GetXAttr{temporaryDirAttr.value().uuid(),
                    ONEDATA_S3_XATTR_CONTENT_TYPE});

            PUSH_FUTURES_7(bucketAttr, firstPartAttr, lastPartAttr,
                temporaryDirAttr, attrs, contentType, multipartETagMd5);
        })
        //
        // Remove the temporary upload files
        //
        .thenValue([this, uploadId](auto &&args) {
            POP_FUTURES_7(args, bucketAttr, firstPartAttr, lastPartAttr,
                temporaryDirAttr, attrs, contentType, multipartETagMd5);

            std::vector<folly::Future<FuseResponse>> futs;

            futs.emplace_back(communicate<FuseResponse>(SetXAttr{
                firstPartAttr.value().uuid(), ONEDATA_S3_XATTR_CONTENT_TYPE,
                contentType.value().value(), false, false}));

            for (const auto &attr : attrs.value().childrenAttrs()) {
                futs.emplace_back(
                    communicate(DeleteFile{attr.uuid().toStdString()}));
            }

            auto futsStatus = folly::collectAll(std::move(futs));

            PUSH_FUTURES_4(
                futsStatus, temporaryDirAttr, multipartETagMd5, contentType);
        })
        //
        // Delete the multipart upload temporary directory
        //
        .thenValue([this, uploadId](auto &&args) {
            POP_FUTURES_4(args, futsStatus, temporaryDirAttr, multipartETagMd5,
                contentType);

            auto deleteStatus = communicate(
                DeleteFile{temporaryDirAttr.value().uuid().toStdString()});

            PUSH_FUTURES_3(multipartETagMd5, contentType, deleteStatus);
        })
        //
        // Complete the multipart upload
        //
        .thenValue([this, uploadId](auto &&args) {
            POP_FUTURES_3(args, multipartETagMd5, contentType, deleteStatus);

            auto completeStatus =
                communicate(CompleteMultipartUpload{uploadId.toStdString()});

            PUSH_FUTURES_3(multipartETagMd5, contentType, completeStatus);
        })
        //
        // Generate S3 CompleteMultipartUploadResult response
        //
        .thenValue([path, bucket](auto &&args) {
            POP_FUTURES_3(args, multipartETagMd5, contentType, deleteStatus);

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

    return communicate<MultipartParts>(
        ListMultipartParts{uploadId.toStdString(), maxParts, partMarker})
        .thenTry([=](folly::Try<MultipartParts> &&parts) {
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
            return communicate<MultipartUploads>(
                ListMultipartUploads{attr.uuid(), maxUploads, indexToken});
        })
        .thenValue([bucket](MultipartUploads &&msg) {
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

} // namespace s3
} // namespace one