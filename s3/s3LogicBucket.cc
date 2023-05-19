/**
 * @file s3LogicBucket.cc
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
#include "messages/fuse/getFileChildrenAttrs.h"
#include "messages/fuse/listFilesRecursively.h"
#include "messages/fuse/multipartParts.h"

#include <spdlog/spdlog.h>

#include <tuple>
#include <utility>

namespace one {
namespace s3 {

using one::messages::fuse::FileAttr;
using one::messages::fuse::FileChildrenAttrs;
using one::messages::fuse::FileList;
using one::messages::fuse::GetFileChildrenAttrs;
using one::messages::fuse::ListFilesRecursively;

folly::Future<Aws::S3::Model::ListBucketsResult> S3Logic::listBuckets()
{
    folly::Optional<folly::fbstring> indexToken;
    constexpr auto kMaxFetchSize{10000};

    GetFileChildrenAttrs getFileChildrenAttrs{
        m_rootUuid, 0, kMaxFetchSize, indexToken, false, false};

    return communicate<FileChildrenAttrs>(std::move(getFileChildrenAttrs))
        .then(&S3Logic::toListBucketsResult, this);
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

folly::Future<Aws::S3::Model::ListObjectsResult> S3Logic::readDir(
    const folly::fbstring &bucket, const folly::fbstring &prefix,
    const folly::Optional<folly::fbstring> &marker,
    const folly::fbstring & /*delimiter*/, const size_t maxKeys)
{
    return getBucketAttr(bucket)
        .thenValue([this, prefix](FileAttr &&attr) {
            if (prefix.empty())
                return folly::makeFuture<FileAttr>(std::move(attr));

            return getFileAttr(attr.uuid(), prefix);
        })
        .thenTry([this, maxKeys, marker](auto &&attr) {
            if (attr.hasException()) {
                auto attrs = FileChildrenAttrs{};
                PUSH_FUTURES_2(attrs, true);
            }

            if (attr.value().type() == FileAttr::FileType::directory) {
                auto attrs = communicate<FileChildrenAttrs>(
                    GetFileChildrenAttrs{attr.value().uuid(), 0, maxKeys,
                        marker, false, false, {ONEDATA_S3_XATTR_CONTENT_MD5}});

                PUSH_FUTURES_2(attrs, false);
            }

            auto attrs = FileChildrenAttrs{std::move(attr.value())};
            PUSH_FUTURES_2(attrs, true);
        })
        .thenValue([prefix, bucket, marker, maxKeys](auto &&args) {
            POP_FUTURES_2(args, attrs, isPrefixARegularFilePath);

            Aws::S3::Model::ListObjectsResult result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);

            int keyCount{0};
            for (const auto &attr : attrs.value().childrenAttrs()) {
                folly::fbstring prefixPrefix{prefix};
                if (!prefixPrefix.empty() &&
                    !isPrefixARegularFilePath.value() &&
                    prefixPrefix.back() != '/')
                    prefixPrefix += "/";

                if (attr.type() == FileAttr::FileType::directory) {
                    if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX_OLD) == 0)
                        continue;

                    Aws::S3::Model::CommonPrefix cp;
                    cp.SetPrefix(prefixPrefix.toStdString() +
                        attr.name().toStdString() + "/");
                    result.AddCommonPrefixes(std::move(cp));
                }
                else {
                    Aws::S3::Model::Object object;
                    if (!isPrefixARegularFilePath.value())
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
            if (attrs.value().isLast().has_value())
                isTruncated = !attrs.value().isLast().value();

            result.SetIsTruncated(isTruncated);

            if (marker.has_value())
                result.SetMarker(marker.value().toStdString());

            if (isTruncated && attrs.value().indexToken()) {
                result.SetNextMarker(
                    attrs.value().indexToken().value().toStdString());
            }

            return result;
        });
}

folly::Future<Aws::S3::Model::ListObjectsV2Result> S3Logic::readDirV2(
    const folly::fbstring &bucket, const folly::fbstring &prefix,
    const folly::Optional<folly::fbstring> &marker,
    const folly::fbstring & /*delimiter*/, const size_t maxKeys)
{
    return getBucketAttr(bucket)
        .thenValue([this, prefix](FileAttr &&attr) {
            if (prefix.empty())
                return folly::makeFuture<FileAttr>(std::move(attr));

            return getFileAttr(attr.uuid(), prefix);
        })
        .thenTry([this, maxKeys, marker](folly::Try<FileAttr> &&maybeAttr) {
            if (maybeAttr.hasException()) {
                auto attrs = FileChildrenAttrs{};
                PUSH_FUTURES_2(attrs, true);
            }

            auto attr = std::move(maybeAttr).value();

            if (attr.type() == FileAttr::FileType::directory) {
                auto attrs =
                    communicate<FileChildrenAttrs>(GetFileChildrenAttrs{
                        attr.uuid(), 0, maxKeys, marker, false, false});
                PUSH_FUTURES_2(attrs, false);
            }

            auto attrs = FileChildrenAttrs{std::move(attr)};
            PUSH_FUTURES_2(attrs, true);
        })
        .thenValue([prefix, bucket, marker, maxKeys](auto &&args) {
            POP_FUTURES_2(args, attrs, isPrefixARegularFilePath);

            Aws::S3::Model::ListObjectsV2Result result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);

            int keyCount{0};
            for (const auto &attr : attrs.value().childrenAttrs()) {
                folly::fbstring prefixPrefix{prefix};
                if (!prefixPrefix.empty() &&
                    !isPrefixARegularFilePath.value() &&
                    prefixPrefix.back() != '/')
                    prefixPrefix += "/";

                if (attr.type() == FileAttr::FileType::directory) {
                    if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX_OLD) == 0)
                        continue;

                    Aws::S3::Model::CommonPrefix cp;
                    cp.SetPrefix(prefixPrefix.toStdString() +
                        attr.name().toStdString() + "/");
                    result.AddCommonPrefixes(std::move(cp));
                }
                else {
                    Aws::S3::Model::Object object;
                    if (!isPrefixARegularFilePath.value())
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
            if (attrs.value().isLast().has_value())
                isTruncated = !attrs.value().isLast().value();
            result.SetIsTruncated(isTruncated);

            if (marker.has_value()) {
                result.SetContinuationToken(marker.value().toStdString());
            }

            if (isTruncated && attrs.value().indexToken()) {
                result.SetNextContinuationToken(
                    attrs.value().indexToken().value().toStdString());
            }

            return result;
        });
}

folly::Future<Aws::S3::Model::ListObjectsV2Result> S3Logic::readDirV2Recursive(
    const folly::fbstring &bucket, const folly::fbstring &prefix,
    const folly::Optional<folly::fbstring> &marker, const size_t maxKeys,
    const bool includeDirectories)
{
    return getBucketAttr(bucket)
        .thenValue(
            [this, maxKeys, marker, includeDirectories, prefix](auto &&attr) {
                return communicate<FileList>(ListFilesRecursively{attr.uuid(),
                    maxKeys, marker, {}, prefix, {ONEDATA_S3_XATTR_CONTENT_MD5},
                    includeDirectories});
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
                if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX_OLD) == 0)
                    continue;
                if (attr.name() == ".")
                    continue;

                if (attr.type() == FileAttr::FileType::directory) {
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
                            // The previous directory is empty it - add it
                            // now
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
    return getBucketAttr(bucket)
        .thenValue([this, maxKeys, marker, prefix](auto &&attr) {
            return communicate<FileList>(ListFilesRecursively{attr.uuid(),
                maxKeys, marker, {}, prefix, {ONEDATA_S3_XATTR_CONTENT_MD5}});
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
                if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX_OLD) == 0)
                    continue;
                if (attr.name() == ".")
                    continue;

                if (attr.type() == FileAttr::FileType::directory) {
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
                            // The previous directory is empty it - add it
                            // now
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

} // namespace s3
} // namespace one