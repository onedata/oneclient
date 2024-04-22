/**
 * @file s3LogicObject.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Logic.h"

#include "futureUtils.h"
#include "messages/fuse/createPath.h"
#include "messages/fuse/deleteFile.h"
#include "messages/fuse/fileChildren.h"
#include "messages/fuse/fileCreated.h"
#include "messages/fuse/fileList.h"
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/getXAttr.h"
#include "messages/fuse/multipartParts.h"
#include "messages/fuse/multipartUpload.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/setXAttr.h"
#include "monitoring/monitoring.h"

#include <spdlog/spdlog.h>

#include <tuple>
#include <utility>

namespace one {
namespace s3 {

using one::client::fslogic::FuseFileHandle;
using one::messages::fuse::DeleteFile;
using one::messages::fuse::FileAttr;
using one::messages::fuse::FileRenamed;
using one::messages::fuse::FuseResponse;
using one::messages::fuse::GetXAttr;
using one::messages::fuse::Rename;
using one::messages::fuse::SetXAttr;
using one::messages::fuse::XAttr;

folly::Future<Aws::S3::Model::HeadObjectResult> S3Logic::headObject(
    const folly::fbstring &bucket, const folly::fbstring &path,
    const std::string &requestId)
{
    if (path.empty()) {
        return headBucket(bucket, requestId);
    }

    return getBucketAttr(bucket, requestId)
        .thenValue([this, path](auto &&attr) {
            return getFileAttrByPath(attr.uuid(), path);
        })
        .thenTry([this, bucket, path, requestId](
                     folly::Try<FileAttr> &&maybeAttr) {
            if (maybeAttr.hasException())
                throw one::s3::error::NoSuchKey(
                    bucket.toStdString(), path.toStdString(), requestId);

            auto attr = std::move(maybeAttr).value();

            const bool isDirectory =
                attr.type() == FileAttr::FileType::directory;
            if (isDirectory && path.back() != '/') {
                throw one::s3::error::NoSuchKey(
                    bucket.toStdString(), path.toStdString(), requestId);
            }

            auto uuid = attr.uuid();

            if (!isDirectory) {
                auto md5Xattr = communicate<XAttr>(
                    GetXAttr{uuid, ONEDATA_S3_XATTR_CONTENT_MD5})
                                    .via(m_executor.get());
                auto contentTypeXattr = communicate<XAttr>(
                    GetXAttr{uuid, ONEDATA_S3_XATTR_CONTENT_TYPE})
                                            .via(m_executor.get());

                PUSH_FUTURES_3(attr, md5Xattr, contentTypeXattr);
            }

            auto md5Xattr =
                folly::makeFuture(XAttr{ONEDATA_S3_XATTR_CONTENT_MD5,
                    fmt::format("\"{}\"",
                        one::client::util::md5::md5(uuid.toStdString()))});

            auto contentTypeXattr = folly::makeFuture(XAttr{
                ONEDATA_S3_XATTR_CONTENT_TYPE, "\"application/octet-stream\""});

            PUSH_FUTURES_3(attr, md5Xattr, contentTypeXattr);
        })
        .thenValue([](auto &&args) {
            POP_FUTURES_3(args, attr, md5Xattr, contentTypeXattr);

            Aws::S3::Model::HeadObjectResult result;
            result.SetContentLength(attr.value().size().value());
            result.SetLastModified(attr.value().mtime());

            if (!md5Xattr.hasException())
                result.SetETag(md5Xattr.value().value());

            if (!contentTypeXattr.hasException()) {
                std::string contentTypeValue;
                one::client::util::xattr::decodeJsonXAttrValue(
                    contentTypeXattr.value().value(), contentTypeValue);
                result.SetContentType(std::move(contentTypeValue));
            }

            return result;
        });
}

folly::Future<std::pair<Aws::S3::Model::GetObjectResult,
    std::pair<std::function<std::size_t(char *, std::size_t)>, std::string>>>
S3Logic::getObject(const folly::fbstring &bucket, const folly::fbstring &path,
    const std::string &requestId,
    const folly::Optional<folly::fbstring> &rangeHeader,
    std::function<void(size_t)> completionCallback,
    std::function<void(const error::S3Exception &)> errorCallback)
{
    return getBucketAttr(bucket, requestId)
        .via(m_executor.get())
        //
        // Get the attribute of the file to download
        //
        .thenTry([this, bucket, path, requestId](auto &&maybeBucketAttr) {
            if (maybeBucketAttr.hasException()) {
                throw one::s3::error::NoSuchBucket(
                    bucket.toStdString(), path.toStdString(), requestId);
            }

            auto &bucketAttr = maybeBucketAttr.value();
            auto attr = getFileAttrByPath(bucketAttr.uuid(), path);

            PUSH_FUTURES_2(bucketAttr, attr);
        })
        //
        // Parse the range header and calculate the offset and size
        // for the request
        //
        .thenValue([bucket, path, rangeHeader, requestId](auto &&args) {
            POP_FUTURES_2(args, bucketAttr, attr);

            if (attr.hasException()) {
                throw one::s3::error::NoSuchKey(
                    bucket.toStdString(), path.toStdString(), requestId);
            }

            if (attr.value().type() == FileAttr::FileType::directory) {
                throw one::s3::error::NoSuchKey(
                    bucket.toStdString(), path.toStdString(), requestId);
            }

            size_t requestOffset{0};
            size_t requestSize{
                static_cast<size_t>(attr.value().size().value())};
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

            PUSH_FUTURES_4(bucketAttr, attr, requestOffset, requestSize);
        })
        //
        // Create a stream reader callback for the request
        //
        .thenValue([this, bucket, path, requestId,
                       completionCallback = std::move(completionCallback),
                       errorCallback](auto &&args) mutable {
            POP_FUTURES_4(args, bucketAttr, attr, requestOffset, requestSize);

            const auto spaceId = bucketAttr.value().uuid();

            std::function<std::size_t(char *, std::size_t)> streamReader;
            // If the requested size is larger than threshold - use
            // streaming Otherwise just return the body content as
            // std::string in a single read.
            if (requestSize.value() > m_options->getOneS3StreamGetThreshold()) {
                streamReader = getRangeStreamReader(bucket, path, requestId,
                    spaceId, requestOffset.value(), requestSize.value(),
                    attr.value(), std::move(completionCallback),
                    std::move(errorCallback));
            }

            // Preopen the file - to make sure that we can
            // and throw EACCESS if not
            auto fileHandleIgnore = open(requestId, spaceId, attr.value(),
                requestOffset.value(), O_RDONLY, requestSize.value())
                                        .via(m_executor.get());

            PUSH_FUTURES_6(bucketAttr, attr, streamReader, requestSize,
                requestOffset, fileHandleIgnore);
        })
        .thenValue([this, bucket, path, requestId, errorCallback](
                       auto &&args) mutable {
            POP_FUTURES_6(args, bucketAttr, attr, streamReader, requestSize,
                requestOffset, fileHandleIgnore);

            if (fileHandleIgnore.hasException()) {
                fileHandleIgnore.exception().with_exception(
                    [bucket, path, requestId](std::system_error &e) {
                        error::S3Exception::raiseFromSystemError(e,
                            bucket.toStdString(), path.toStdString(),
                            requestId);
                    });
            }

            auto spaceId = bucketAttr.value().uuid();

            auto md5Xattr = communicate<XAttr>(
                GetXAttr{attr.value().uuid(), ONEDATA_S3_XATTR_CONTENT_MD5})
                                .thenTry([](folly::Try<XAttr> &&xattr) {
                                    if (xattr.hasException())
                                        return std::string{"\"\""};

                                    return xattr.value().value();
                                });

            auto contentTypeXattr =
                communicate<XAttr>(GetXAttr{attr.value().uuid(),
                                       ONEDATA_S3_XATTR_CONTENT_TYPE})
                    .thenTry([](folly::Try<XAttr> &&xattr) {
                        if (xattr.hasException())
                            return std::string{"\"application/octet-stream\""};

                        return xattr.value().value();
                    });

            auto arg5 = folly::makeFuture(requestSize);

            if (streamReader.value() == nullptr) {
                // Read the entire data range here
                auto responseBodyStr =
                    getRange(bucket, path, requestId, spaceId,
                        requestOffset.value(), requestSize.value(),
                        attr.value())
                        .thenError(folly::tag_t<error::S3Exception>{},
                            [bucket, path, requestId,
                                errorCallback = std::move(errorCallback)](
                                auto &&e) -> std::string {
                                if (errorCallback)
                                    errorCallback(e);
                                return std::string{};
                            });

                PUSH_FUTURES_8(bucketAttr, attr, streamReader, md5Xattr,
                    contentTypeXattr, requestSize, requestOffset,
                    responseBodyStr);
            }

            std::string responseBodyStr{};
            PUSH_FUTURES_8(bucketAttr, attr, streamReader, md5Xattr,
                contentTypeXattr, requestSize, requestOffset, responseBodyStr);
        })
        .thenValue([](auto &&args) {
            POP_FUTURES_8(args, bucketAttr, attr, streamReader, md5Xattr,
                contentTypeXattr, requestSize, requestOffset, responseBodyStr);

            Aws::S3::Model::GetObjectResult result;

            if (md5Xattr.hasValue()) {
                auto md5 = md5Xattr.value();
                std::string md5Value;
                one::client::util::xattr::decodeJsonXAttrValue(md5, md5Value);
                result.SetETag(md5);
            }

            if (contentTypeXattr.hasValue()) {
                auto contentType = contentTypeXattr.value();
                std::string contentTypeValue;
                one::client::util::xattr::decodeJsonXAttrValue(
                    contentType, contentTypeValue);
                result.SetContentType(contentType);
            }

            result.SetContentLength(requestSize.value());
            result.SetLastModified(attr.value().mtime());

            size_t fileSize{0};
            if (attr.value().size()) {
                fileSize = attr.value().size().value();
            }

            if (requestSize.value() < fileSize) {
                result.SetContentRange(
                    fmt::format("bytes {}-{}/{}", requestOffset.value(),
                        std::max<size_t>(
                            0, requestOffset.value() + requestSize.value() - 1),
                        fileSize));
            }

            return folly::makeFuture(std::make_pair(std::move(result),
                std::make_pair(std::move(streamReader.value()),
                    std::move(responseBodyStr.value()))));
        });
}

std::function<std::size_t(char *, std::size_t)> S3Logic::getRangeStreamReader(
    folly::fbstring bucket, folly::fbstring path, std::string requestId,
    folly::fbstring spaceId, const size_t requestOffset,
    const size_t requestSize, const FileAttr &attr,
    std::function<void(size_t)> completionCallback,
    std::function<void(const error::S3Exception &)> errorCallback)
{
    return [this, attr, spaceId = std::move(spaceId),
               bucket = std::move(bucket), requestId = std::move(requestId),
               path = std::move(path), requestOffset, requestSize,
               completionCallback = std::move(completionCallback),
               errorCallback = std::move(errorCallback)](
               char *data, std::size_t size) mutable {
        if (data == nullptr) {
            // Close the file on end of stream
            return close(attr.uuid(), requestId)
                .via(m_executor.get())
                .thenValue(
                    [path, requestSize,
                        completionCallback = std::move(completionCallback)](
                        auto && /*unit*/) -> size_t {
                        if (completionCallback)
                            completionCallback(requestSize);
                        return 0;
                    })
                .get();
        }

        return open(requestId, spaceId, attr, requestOffset, O_RDONLY, size)
            .via(m_executor.get())
            .thenError(folly::tag_t<std::system_error>{},
                [bucket, path, requestId](
                    auto &&e) -> std::shared_ptr<FuseFileHandle> {
                    error::S3Exception::raiseFromSystemError(
                        e, bucket.toStdString(), path.toStdString(), requestId);
                    return {}; // NOLINT
                })
            .thenValue(
                [this, requestId, size, requestOffset, requestSize](
                    std::shared_ptr<FuseFileHandle> &&fileHandle) mutable {
                    return read(std::move(fileHandle), requestId, size,
                        requestOffset, requestSize);
                })
            .thenValue([this, data, requestOffset, size, requestSize,
                           requestId](auto &&buf) mutable {
                if (buf.chainLength() == 0)
                    return 0UL;

                auto iobuf = buf.empty() ? folly::IOBuf::create(0) : buf.move();
                if (iobuf->isChained()) {
                    iobuf->unshare();
                    iobuf->coalesce();
                }

                auto bufSize = iobuf->length();

                LOG_DBG(3) << "[" << requestId << "] Read " << bufSize << "/"
                           << size << "/" << requestSize << " at offset "
                           << requestOffset;

                memcpy(data, iobuf->data(), bufSize);

                m_downloadedBytes.fetch_add(bufSize);

                return bufSize;
            })
            .thenError(folly::tag_t<error::S3Exception>{},
                [bucket, path, requestId,
                    errorCallback = std::move(errorCallback)](
                    auto &&e) -> size_t {
                    if (errorCallback)
                        errorCallback(e);
                    return 0;
                })
            .get();
    };
}

folly::Future<std::string> S3Logic::getRange(const folly::fbstring &bucket,
    const folly::fbstring &path, const std::string &requestId,
    const folly::fbstring &spaceId, const size_t requestOffset,
    const size_t requestSize, const FileAttr &attr)
{
    return open(requestId, spaceId, attr, requestOffset, O_RDONLY, requestSize)
        .via(m_executor.get())
        .thenError(folly::tag_t<std::system_error>{},
            [bucket, path, requestId](
                auto &&e) -> std::shared_ptr<FuseFileHandle> {
                error::S3Exception::raiseFromSystemError(
                    e, bucket.toStdString(), path.toStdString(), requestId);

                return {}; // NOLINT
            })
        .thenValue([this, requestId, requestOffset, requestSize](
                       std::shared_ptr<FuseFileHandle> &&fileHandle) mutable {
            auto buf = read(std::move(fileHandle), requestId, requestSize,
                requestOffset, requestSize);

            PUSH_FUTURES_1(buf);
        })
        .thenValue(
            [this, requestOffset, requestSize, requestId](auto &&args) mutable {
                POP_FUTURES_1(args, buf);

                if (buf.value().chainLength() == 0)
                    return std::string{};

                auto iobuf = buf.value().empty() ? folly::IOBuf::create(0)
                                                 : buf.value().move();
                if (iobuf->isChained()) {
                    iobuf->unshare();
                    iobuf->coalesce();
                }

                auto bufSize = iobuf->length();

                LOG_DBG(3) << "[" << requestId << "] Read " << bufSize << "/"
                           << requestSize << "/" << requestSize << " at offset "
                           << requestOffset;

                auto res = iobuf->moveToFbString().toStdString();

                m_downloadedBytes.fetch_add(bufSize);

                return res;
            })
        .thenValue([this, attr, requestId](std::string &&data) {
            return close(attr.uuid(), requestId)
                .thenValue([data = std::move(data)](
                               auto && /*unit*/) { return data; });
        });
}

folly::Future<size_t> S3Logic::uploadObject(const std::string &requestId,
    const folly::fbstring &bucket, const folly::fbstring &path,
    const folly::fbstring &md5, const folly::fbstring &contentType,
    std::shared_ptr<folly::IOBuf> buf)
{
    return getBucketAttr(bucket, requestId)
        .via(m_executor.get())
        //
        // Get temporary upload directory attr or create it if necessary
        //
        .thenTry([this, path, bucket, requestId](auto &&maybeBucketAttr) {
            if (maybeBucketAttr.hasException()) {
                throw one::s3::error::NoSuchBucket(
                    bucket.toStdString(), path.toStdString(), requestId);
            }

            auto bucketAttr = maybeBucketAttr.value();

            auto tmpDirAttr = getBucketTmpDirAttr(bucketAttr.name(), requestId);

            PUSH_FUTURES_2(bucketAttr, tmpDirAttr);
        })
        .via(m_executor.get())
        //
        // Create temporary file for upload
        //
        .thenValue([this, requestId](auto &&args) {
            POP_FUTURES_2(args, bucketAttr, tmpDirAttr);

            auto filePerms = m_options->getOneS3FileMode();

            folly::fbstring parentUuid = tmpDirAttr.value().uuid();

            const auto tmpFileName =
                getCompleteUploadTemporaryFileName(requestId);

            auto attr = create(requestId, parentUuid, tmpFileName,
                S_IFREG | filePerms, O_WRONLY);

            PUSH_FUTURES_2(bucketAttr, attr);
        })
        //
        // Open the temporary file
        //
        .thenValue([this, requestId, bucket, path](auto &&args) {
            POP_FUTURES_2(args, bucketAttr, attr);

            folly::fbstring spaceId = bucketAttr.value().uuid();

            auto fileHandle =
                open(requestId, spaceId, attr.value(), 0UL, O_WRONLY);

            PUSH_FUTURES_3(bucketAttr, attr, fileHandle);
        })
        //
        // Write data to the temporary file and create target path if
        // necessary
        //
        .thenValue(
            [this, requestId, path, buf = std::move(buf)](auto &&args) mutable {
                POP_FUTURES_3(args, bucketAttr, attr, fileHandle);

                folly::fbstring spaceId = attr.value().uuid();
                auto targetParentAttr =
                    getFileParentAttrByPath(bucketAttr.value(), path);
                auto written = write(
                    fileHandle.value(), spaceId, requestId, std::move(buf));

                PUSH_FUTURES_4(attr, targetParentAttr, written, fileHandle);
            })
        //
        // Release temporary file handle
        //
        .thenValue([this, requestId, bucket, path, buf = std::move(buf)](
                       auto &&args) {
            POP_FUTURES_4(args, attr, targetParentAttr, written, fileHandle);

            auto closeStatus = close(attr.value().uuid(), requestId);

            PUSH_FUTURES_4(attr, targetParentAttr, written, closeStatus);
        })
        //
        // Set the md5 checksum and content type as xattr on the file
        //
        .thenValue([this, md5, contentType](auto &&args) {
            POP_FUTURES_4(args, attr, targetParentAttr, written, closeStatus);

            auto setContentMD5Status = communicate<FuseResponse>(
                SetXAttr{attr.value().uuid(), ONEDATA_S3_XATTR_CONTENT_MD5,
                    fmt::format("\"{}\"", md5.toStdString()), false, false});

            auto setContentTypeStatus = communicate<FuseResponse>(
                SetXAttr{attr.value().uuid(), ONEDATA_S3_XATTR_CONTENT_TYPE,
                    fmt::format("\"{}\"", contentType.toStdString()), false,
                    false});

            PUSH_FUTURES_5(attr, targetParentAttr, written, setContentMD5Status,
                setContentTypeStatus);
        })
        //
        // Rename the temporary file to the target file
        //
        .thenValue([this, requestId, bucket, path](auto &&args) {
            POP_FUTURES_3(args, attr, targetParentAttr, written);

            const auto &oldUuid = attr.value().uuid().toStdString();
            const auto &newParentUuid =
                targetParentAttr.value().uuid().toStdString();
            const auto &newName = getFileNameFromPath(path).toStdString();

            auto renamedStatus = communicate<FileRenamed>(
                Rename{oldUuid, newParentUuid, newName});

            m_uploadedBytes.fetch_add(written.value());

            PUSH_FUTURES_2(written, renamedStatus);
        })
        .thenValue([](auto &&args) {
            POP_FUTURES_2(args, written, renamedStatus);
            return written.value();
        });
}

folly::Future<folly::Unit> S3Logic::deleteObject(const std::string &requestId,
    const folly::fbstring &bucket, const folly::fbstring &path)
{
    return getBucketAttr(bucket, requestId)
        .thenValue([this, path](auto &&attr) {
            return getFileAttrByPath(attr.uuid(), path);
        })
        .thenTry([this](folly::Try<FileAttr> &&value) {
            if (value.hasException()) {
                value.throwIfFailed();
            }

            return communicate(DeleteFile{value.value().uuid().toStdString()});
        })
        .thenValue([](auto && /*msg*/) { return folly::Unit{}; });
}

} // namespace s3
} // namespace one