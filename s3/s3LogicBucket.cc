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

#include <aws/core/http/URI.h>
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

Aws::String encodeURLPath(const std::string &path)
{
    auto result = Aws::Http::URI::URLEncodePathRFC3986(path);

    if (!result.empty() && result[0] == '/')
        return result.substr(1);

    return result;
}

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
    const folly::fbstring &bucket, const std::string &requestId)
{
    return getBucketAttr(bucket, requestId).thenValue([](auto &&attr) {
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
    const folly::fbstring & /*delimiter*/, const size_t maxKeys,
    const std::string &requestId)
{
    return getBucketAttr(bucket, requestId)
        .thenValue([this, prefix](FileAttr &&attr) {
            if (prefix.empty() || prefix == "/")
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

            attrs.throwUnlessValue();

            Aws::S3::Model::ListObjectsResult result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);
            result.SetDelimiter("/");

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
                    cp.SetPrefix(encodeURLPath(prefixPrefix.toStdString() +
                        attr.name().toStdString() + "/"));
                    result.AddCommonPrefixes(std::move(cp));
                }
                else {
                    Aws::S3::Model::Object object;
                    if (!isPrefixARegularFilePath.value())
                        object.SetKey(encodeURLPath(prefixPrefix.toStdString() +
                            attr.name().toStdString()));
                    else
                        object.SetKey(
                            encodeURLPath(prefixPrefix.toStdString()));
                    object.SetLastModified(attr.mtime());
                    if (attr.has_xattr(ONEDATA_S3_XATTR_CONTENT_MD5)) {
                        object.SetETag(
                            attr.xattr(ONEDATA_S3_XATTR_CONTENT_MD5));
                    }
                    object.SetSize(*attr.size());
                    result.AddContents(std::move(object));
                }
            }

            bool isTruncated{true};
            if (attrs.value().isLast().has_value())
                isTruncated = !attrs.value().isLast().value();
            if (maxKeys == 0)
                isTruncated = false;
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
    const folly::fbstring & /*delimiter*/, const size_t maxKeys,
    const std::string &requestId)
{
    return getBucketAttr(bucket, requestId)
        .thenValue([this, prefix](FileAttr &&attr) {
            if (prefix.empty() || prefix == "/")
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

            attrs.throwUnlessValue();

            Aws::S3::Model::ListObjectsV2Result result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);
            result.SetDelimiter("/");

            size_t keyCount{0};
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
                    cp.SetPrefix(encodeURLPath(prefixPrefix.toStdString() +
                        attr.name().toStdString() + "/"));
                    result.AddCommonPrefixes(std::move(cp));
                    keyCount++;
                }
                else {
                    Aws::S3::Model::Object object;
                    if (!isPrefixARegularFilePath.value())
                        object.SetKey(encodeURLPath(prefixPrefix.toStdString() +
                            attr.name().toStdString()));
                    else
                        object.SetKey(
                            encodeURLPath(prefixPrefix.toStdString()));

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
    const folly::Optional<folly::fbstring> &token,
    const folly::Optional<folly::fbstring> &startAfter, const size_t maxKeys,
    const bool fetchOwner, const std::string &requestId,
    const bool includeDirectories)
{
    return getBucketAttr(bucket, requestId)
        .thenTry([this, maxKeys, startAfter, includeDirectories, token, prefix](
                     auto &&maybeAttr) {
            maybeAttr.throwUnlessValue();
            return communicate<FileList>(ListFilesRecursively{
                maybeAttr.value().uuid(), maxKeys, token, startAfter, prefix,
                {ONEDATA_S3_XATTR_CONTENT_MD5}, includeDirectories});
        })
        .thenValue([prefix, bucket, token, startAfter, maxKeys, fetchOwner](
                       FileList &&attrs) {
            Aws::S3::Model::ListObjectsV2Result result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);
            if (startAfter.has_value())
                result.SetStartAfter(startAfter.value().toStdString());

            int keyCount{0};
            bool resultContainedDotDirectory{false};
            bool lastItemWasDirectory{false};
            Aws::S3::Model::Object dirObject;
            dirObject.SetSize(0);

            for (const auto &attr : attrs.files()) {
                if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX_OLD) == 0)
                    continue;
                if (attr.name() == ".") {
                    resultContainedDotDirectory = true;
                    continue;
                }

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

                    if (fetchOwner) {
                        Aws::S3::Model::Owner owner{};
                        owner.SetID(attr.ownerId());
                        dirObject.SetOwner(std::move(owner));
                    }

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

                    if (fetchOwner) {
                        Aws::S3::Model::Owner owner{};
                        owner.SetID(attr.ownerId());
                        object.SetOwner(std::move(owner));
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
            if (maxKeys == 1 && resultContainedDotDirectory)
                isTruncated = false;
            result.SetIsTruncated(isTruncated);
            if (!isTruncated && lastItemWasDirectory) {
                result.AddContents(std::move(dirObject));
            }

            if (resultContainedDotDirectory && maxKeys > 0)
                result.SetMaxKeys(maxKeys - 1); // NOLINT

            if (token.has_value())
                result.SetContinuationToken(token.value().toStdString());

            if (attrs.nextPageToken())
                result.SetNextContinuationToken(
                    attrs.nextPageToken().value().toStdString());

            return result;
        });
}

folly::Future<Aws::S3::Model::ListObjectsResult> S3Logic::readDirRecursive(
    const folly::fbstring &bucket, const folly::fbstring &prefix,
    const folly::Optional<folly::fbstring> &token,
    const folly::Optional<folly::fbstring> &startAfter, const size_t maxKeys,
    const std::string &requestId)
{
    const auto effectiveMaxKeys = maxKeys;

    return getBucketAttr(bucket, requestId)
        .thenValue([this, maxKeys = effectiveMaxKeys, token, startAfter,
                       prefix](auto &&attr) {
            return communicate<FileList>(
                ListFilesRecursively{attr.uuid(), maxKeys, token, startAfter,
                    prefix, {ONEDATA_S3_XATTR_CONTENT_MD5}});
        })
        .thenValue([prefix, startAfter, bucket, token,
                       maxKeys = effectiveMaxKeys](FileList &&attrs) {
            Aws::S3::Model::ListObjectsResult result;
            result.SetPrefix(prefix.toStdString());
            result.SetName(bucket.toStdString());
            result.SetMaxKeys(maxKeys);
            bool resultContainedDotDirectory{false};

            bool lastItemWasDirectory{false};
            Aws::S3::Model::Object dirObject;
            dirObject.SetSize(0);

            for (const auto &attr : attrs.files()) {
                if (attr.name().find(ONEDATA_S3_MULTIPART_PREFIX_OLD) == 0)
                    continue;
                if (attr.name() == ".") {
                    resultContainedDotDirectory = true;
                    continue;
                }

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
            if (maxKeys == 1 && resultContainedDotDirectory)
                isTruncated = false;
            result.SetIsTruncated(isTruncated);

            if (!isTruncated && lastItemWasDirectory) {
                result.AddContents(std::move(dirObject));
            }

            if (resultContainedDotDirectory && maxKeys > 0)
                result.SetMaxKeys(maxKeys - 1); // NOLINT

            const std::string kOnes3MarkerPrefix{".__onedata__ones3marker__#"};

            if (startAfter.has_value() && !startAfter.value().empty())
                result.SetMarker(startAfter.value().toStdString());
            else if (token.has_value()) {
                result.SetMarker(
                    kOnes3MarkerPrefix + token.value().toStdString());
            }

            if (attrs.nextPageToken())
                result.SetNextMarker(kOnes3MarkerPrefix +
                    attrs.nextPageToken().value().toStdString());

            return result;
        });
}

} // namespace s3
} // namespace one