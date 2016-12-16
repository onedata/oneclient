/**
 * @file s3Helper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Helper.h"
#include "logging.h"

#include <aws/core/auth/AWSCredentialsProvider.h>
#include <aws/core/client/ClientConfiguration.h>
#include <aws/s3/S3Client.h>
#include <aws/s3/model/Delete.h>
#include <aws/s3/model/DeleteObjectsRequest.h>
#include <aws/s3/model/GetObjectRequest.h>
#include <aws/s3/model/ListObjectsRequest.h>
#include <aws/s3/model/PutObjectRequest.h>
#include <boost/algorithm/string.hpp>
#include <folly/Range.h>
#include <glog/stl_logging.h>

#include <algorithm>

namespace {

std::map<Aws::S3::S3Errors, std::errc> g_errors = {
    {Aws::S3::S3Errors::INVALID_PARAMETER_VALUE, std::errc::invalid_argument},
    {Aws::S3::S3Errors::MISSING_ACTION, std::errc::not_supported},
    {Aws::S3::S3Errors::SERVICE_UNAVAILABLE, std::errc::host_unreachable},
    {Aws::S3::S3Errors::NETWORK_CONNECTION, std::errc::network_unreachable},
    {Aws::S3::S3Errors::REQUEST_EXPIRED, std::errc::timed_out},
    {Aws::S3::S3Errors::ACCESS_DENIED, std::errc::permission_denied},
    {Aws::S3::S3Errors::UNKNOWN, std::errc::no_such_file_or_directory},
    {Aws::S3::S3Errors::NO_SUCH_BUCKET, std::errc::no_such_file_or_directory},
    {Aws::S3::S3Errors::NO_SUCH_KEY, std::errc::no_such_file_or_directory},
    {Aws::S3::S3Errors::RESOURCE_NOT_FOUND,
        std::errc::no_such_file_or_directory}};

template <typename Outcome>
std::error_code getReturnCode(const Outcome &outcome)
{
    if (outcome.IsSuccess())
        return one::helpers::SUCCESS_CODE;

    auto error = std::errc::io_error;
    auto search = g_errors.find(outcome.GetError().GetErrorType());
    if (search != g_errors.end())
        error = search->second;

    return std::error_code(static_cast<int>(error), std::system_category());
}

template <typename Outcome>
void throwOnError(const folly::fbstring &operation, const Outcome &outcome)
{
    auto code = getReturnCode(outcome);
    if (!code)
        return;

    auto msg =
        operation.toStdString() + "': " + outcome.GetError().GetMessage();

    throw std::system_error{code, std::move(msg)};
}

} // namespace

namespace one {
namespace helpers {

S3Helper::S3Helper(folly::fbstring hostname, folly::fbstring bucketName,
    folly::fbstring accessKey, folly::fbstring secretKey, const bool useHttps,
    Timeout timeout)
    : m_bucket{std::move(bucketName)}
    , m_timeout{std::move(timeout)}
{
    static S3HelperApiInit init;

    Aws::Auth::AWSCredentials credentials{
        accessKey.toStdString(), secretKey.toStdString()};

    Aws::Client::ClientConfiguration configuration;
    configuration.endpointOverride = hostname.toStdString();
    if (!useHttps)
        configuration.scheme = Aws::Http::Scheme::HTTP;

    m_client = std::make_unique<Aws::S3::S3Client>(credentials, configuration);
}

folly::IOBufQueue S3Helper::getObject(
    const folly::fbstring &key, const off_t offset, const std::size_t size)
{
    folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
    char *data = static_cast<char *>(buf.preallocate(size, size).first);

    Aws::S3::Model::GetObjectRequest request;
    request.SetBucket(m_bucket.toStdString());
    request.SetKey(key.toStdString());
    request.SetRange(
        rangeToString(offset, static_cast<off_t>(offset + size - 1)));
    request.SetResponseStreamFactory([ data = data, size ] {
        auto stream = new std::stringstream;
        stream->rdbuf()->pubsetbuf(data, size);
        return stream;
    });

    auto outcome = m_client->GetObject(request);
    auto code = getReturnCode(outcome);
    if (code != SUCCESS_CODE)
        throwOnError("GetObject", outcome);

    buf.postallocate(
        static_cast<std::size_t>(outcome.GetResult().GetContentLength()));

    return buf;
}

off_t S3Helper::getObjectsSize(
    const folly::fbstring &prefix, const std::size_t objectSize)
{
    Aws::S3::Model::ListObjectsRequest request;
    request.SetBucket(m_bucket.toStdString());
    request.SetPrefix(adjustPrefix(prefix));
    request.SetDelimiter(OBJECT_DELIMITER);
    request.SetMaxKeys(1);

    auto outcome = m_client->ListObjects(request);
    throwOnError("ListObjects", outcome);

    if (outcome.GetResult().GetContents().empty())
        return 0;

    auto key = outcome.GetResult().GetContents().back().GetKey();

    return getObjectId(std::move(key)) * objectSize +
        outcome.GetResult().GetContents().back().GetSize();
}

std::size_t S3Helper::putObject(
    const folly::fbstring &key, folly::IOBufQueue buf)
{
    auto iobuf = buf.empty() ? folly::IOBuf::create(0) : buf.move();
    if (iobuf->isChained()) {
        iobuf->unshare();
        iobuf->coalesce();
    }

    Aws::S3::Model::PutObjectRequest request;
    auto size = iobuf->length();
    auto stream = std::make_shared<std::stringstream>();
    stream->rdbuf()->pubsetbuf(
        reinterpret_cast<char *>(iobuf->writableData()), size);
    request.SetBucket(m_bucket.toStdString());
    request.SetKey(key.toStdString());
    request.SetContentLength(size);
    request.SetBody(stream);

    auto outcome = m_client->PutObject(request);
    throwOnError("PutObject", outcome);

    return size;
}

void S3Helper::deleteObjects(const folly::fbvector<folly::fbstring> &keys)
{
    Aws::S3::Model::DeleteObjectsRequest request;
    request.SetBucket(m_bucket.toStdString());

    for (auto offset = 0u; offset < keys.size(); offset += MAX_DELETE_OBJECTS) {
        Aws::S3::Model::Delete container;

        const std::size_t batchSize =
            std::min<std::size_t>(keys.size() - offset, MAX_DELETE_OBJECTS);

        for (auto &key : folly::range(keys.begin(), keys.begin() + batchSize))
            container.AddObjects(
                Aws::S3::Model::ObjectIdentifier{}.WithKey(key.toStdString()));

        request.SetDelete(std::move(container));
        auto outcome = m_client->DeleteObjects(request);
        throwOnError("DeleteObjects", outcome);
    }
}

folly::fbvector<folly::fbstring> S3Helper::listObjects(
    const folly::fbstring &prefix)
{
    Aws::S3::Model::ListObjectsRequest request;
    request.SetBucket(m_bucket.toStdString());
    request.SetPrefix(adjustPrefix(prefix));
    request.SetDelimiter(OBJECT_DELIMITER);

    folly::fbvector<folly::fbstring> keys;

    while (true) {
        auto outcome = m_client->ListObjects(request);
        throwOnError("ListObjects", outcome);

        for (const auto &object : outcome.GetResult().GetContents())
            keys.emplace_back(object.GetKey());

        if (!outcome.GetResult().GetIsTruncated())
            return keys;

        request.SetMarker(outcome.GetResult().GetNextMarker());
    }
}

} // namespace helpers
} // namespace one
