/**
 * @file s3Helper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Helper.h"
#include "logging.h"
#include "monitoring/monitoring.h"

#include <aws/core/auth/AWSCredentialsProvider.h>
#include <aws/core/client/ClientConfiguration.h>
#include <aws/s3/S3Client.h>
#include <aws/s3/model/Delete.h>
#include <aws/s3/model/DeleteObjectRequest.h>
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
    LOG_FCALL();
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

    auto msg = operation.toStdString() +
        "': " + outcome.GetError().GetMessage().c_str();

    LOG(ERROR) << "Operation " << operation << " failed with message " << msg;

    throw std::system_error{code, std::move(msg)};
}

} // namespace

namespace one {
namespace helpers {

S3Helper::S3Helper(folly::fbstring hostname, folly::fbstring bucketName,
    folly::fbstring accessKey, folly::fbstring secretKey, const bool useHttps,
    const bool useSigV2, Timeout timeout)
    : m_bucket{std::move(bucketName)}
    , m_useSigV2{useSigV2}
    , m_timeout{std::move(timeout)}
{
    LOG_FCALL() << LOG_FARG(hostname) << LOG_FARG(bucketName)
                << LOG_FARG(accessKey) << LOG_FARG(secretKey)
                << LOG_FARG(useHttps) << LOG_FARG(useSigV2)
                << LOG_FARG(timeout.count());

    static S3HelperApiInit init;

    Aws::Auth::AWSCredentials credentials{accessKey.c_str(), secretKey.c_str()};

    Aws::Client::ClientConfiguration configuration;
    configuration.region = getRegion(hostname).c_str();
    configuration.endpointOverride = hostname.c_str();
#if !defined(S3_HAS_NO_V2_SUPPORT)
    configuration.useSigV2 = useSigV2;
#endif
    if (!useHttps)
        configuration.scheme = Aws::Http::Scheme::HTTP;

    m_client =
        std::make_unique<Aws::S3::S3Client>(credentials, configuration, false
#if !defined(S3_HAS_NO_V2_SUPPORT)
            ,
            false
#endif
        );
}

folly::fbstring S3Helper::getRegion(const folly::fbstring &hostname)
{
    LOG_FCALL() << LOG_FARG(hostname);

    folly::fbvector<folly::fbstring> regions{"us-east-2", "us-east-1",
        "us-west-1", "us-west-2", "ca-central-1", "ap-south-1",
        "ap-northeast-2", "ap-southeast-1", "ap-southeast-2", "ap-northeast-1",
        "eu-central-1", "eu-west-1", "eu-west-2", "sa-east-1"};

    LOG_DBG(1) << "Attempting to determine S3 region based on hostname";

    for (const auto &region : regions) {
        if (hostname.find(region) != folly::fbstring::npos) {
            LOG_DBG(1) << "Using region " << region;
            return region;
        }
    }

    LOG_DBG(1) << "Using default region us-east-1";

    return "us-east-1";
}

folly::IOBufQueue S3Helper::getObject(
    const folly::fbstring &key, const off_t offset, const std::size_t size)
{
    LOG_FCALL() << LOG_FARG(key) << LOG_FARG(offset) << LOG_FARG(size);

    folly::IOBufQueue buf{folly::IOBufQueue::cacheChainLength()};
    char *data = static_cast<char *>(buf.preallocate(size, size).first);

    Aws::S3::Model::GetObjectRequest request;
    request.SetBucket(m_bucket.c_str());
    request.SetKey(key.c_str());
    request.SetRange(
        rangeToString(offset, static_cast<off_t>(offset + size - 1)).c_str());
    request.SetResponseStreamFactory([ data = data, size ] {
        auto stream = new std::stringstream;
#if !defined(__APPLE__)
        /**
         * pubsetbuf() implementation depends on libstdc++, and on some
         * platforms including OSX it does not work and data must be copied
         */
        stream->rdbuf()->pubsetbuf(data, size);
#endif
        return stream;
    });

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.s3.read");

    LOG_DBG(1) << "Attempting to get " << size << "bytes from object " << key
               << " at offset " << offset;

    auto outcome = m_client->GetObject(request);
    auto code = getReturnCode(outcome);
    if (code != SUCCESS_CODE) {
        LOG_DBG(1) << "Reading from object " << key << " failed with error "
                   << outcome.GetError().GetMessage();
        throwOnError("GetObject", outcome);
    }

#if defined(__APPLE__)
    outcome.GetResult().GetBody().rdbuf()->sgetn(
        data, outcome.GetResult().GetContentLength());
#endif

    auto readBytes = outcome.GetResult().GetContentLength();
    buf.postallocate(static_cast<std::size_t>(readBytes));

    LOG_DBG(1) << "Got " << readBytes << " from object " << key;

    ONE_METRIC_TIMERCTX_STOP(timer, readBytes);

    return buf;
}

off_t S3Helper::getObjectsSize(
    const folly::fbstring &prefix, const std::size_t objectSize)
{
    LOG_FCALL() << LOG_FARG(prefix) << LOG_FARG(objectSize);

    Aws::S3::Model::ListObjectsRequest request;
    request.SetBucket(m_bucket.c_str());
    request.SetPrefix(adjustPrefix(prefix).c_str());
    request.SetDelimiter(OBJECT_DELIMITER);
    request.SetMaxKeys(1);

    LOG_DBG(1) << "Attempting to get size of object at prefix " << prefix;

    auto outcome = m_client->ListObjects(request);
    throwOnError("ListObjects", outcome);

    if (outcome.GetResult().GetContents().empty())
        return 0;

    auto key = folly::fbstring(
        outcome.GetResult().GetContents().back().GetKey().c_str());

    return getObjectId(std::move(key)) * objectSize +
        outcome.GetResult().GetContents().back().GetSize();
}

std::size_t S3Helper::putObject(
    const folly::fbstring &key, folly::IOBufQueue buf)
{
    LOG_FCALL() << LOG_FARG(key) << LOG_FARG(buf.chainLength());

    auto iobuf = buf.empty() ? folly::IOBuf::create(0) : buf.move();
    if (iobuf->isChained()) {
        iobuf->unshare();
        iobuf->coalesce();
    }

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.s3.write");

    Aws::S3::Model::PutObjectRequest request;
    auto size = iobuf->length();
    request.SetBucket(m_bucket.c_str());
    request.SetKey(key.c_str());
    request.SetContentLength(size);
    auto stream = std::make_shared<std::stringstream>();
#if !defined(__APPLE__)
    stream->rdbuf()->pubsetbuf(
        reinterpret_cast<char *>(iobuf->writableData()), size);
#else
    stream->rdbuf()->sputn(const_cast<const char *>(
                               reinterpret_cast<char *>(iobuf->writableData())),
        size);
#endif
    request.SetBody(stream);

    LOG_DBG(1) << "Attempting to write object " << key << " of size " << size;

    auto outcome = m_client->PutObject(request);

    ONE_METRIC_TIMERCTX_STOP(timer, size);

    throwOnError("PutObject", outcome);

    LOG_DBG(1) << "Written " << size << " bytes to object " << key;

    return size;
}

void S3Helper::deleteObjects(const folly::fbvector<folly::fbstring> &keys)
{
    LOG_FCALL() << LOG_FARGV(keys);

#if !defined(S3_HAS_NO_V2_SUPPORT)
    if (m_useSigV2) {
#else
    if (false && m_useSigV2) {
#endif
        for (const auto &key : keys) {
            Aws::S3::Model::DeleteObjectRequest request;
            request.SetBucket(m_bucket.c_str());
            request.SetKey(key.c_str());
            LOG_DBG(1) << "Deleting object " << key;
            auto outcome = m_client->DeleteObject(request);
            throwOnError("DeleteObject", outcome);
        }
    }
    else {
        Aws::S3::Model::DeleteObjectsRequest request;
        request.SetBucket(m_bucket.c_str());

        LOG_DBG(1) << "Attempting to delete objects " << LOG_VEC(keys);

        for (auto offset = 0u; offset < keys.size();
             offset += MAX_DELETE_OBJECTS) {
            Aws::S3::Model::Delete container;

            const std::size_t batchSize =
                std::min<std::size_t>(keys.size() - offset, MAX_DELETE_OBJECTS);

            for (auto &key :
                folly::range(keys.begin(), keys.begin() + batchSize))
                container.AddObjects(
                    Aws::S3::Model::ObjectIdentifier{}.WithKey(key.c_str()));

            request.SetDelete(std::move(container));
            auto outcome = m_client->DeleteObjects(request);
            throwOnError("DeleteObjects", outcome);
        }
    }
}

folly::fbvector<folly::fbstring> S3Helper::listObjects(
    const folly::fbstring &prefix)
{
    LOG_FCALL() << LOG_FARG(prefix);

    Aws::S3::Model::ListObjectsRequest request;
    request.SetBucket(m_bucket.c_str());
    request.SetPrefix(adjustPrefix(prefix).c_str());
    request.SetDelimiter(OBJECT_DELIMITER);

    folly::fbvector<folly::fbstring> keys;

    auto timer = ONE_METRIC_TIMERCTX_CREATE("comp.helpers.mod.s3.listobjects");

    LOG_DBG(1) << "Retrieving object list for prefix " << prefix;

    while (true) {
        auto outcome = m_client->ListObjects(request);
        throwOnError("ListObjects", outcome);

        for (const auto &object : outcome.GetResult().GetContents())
            keys.emplace_back(object.GetKey().c_str());

        if (!outcome.GetResult().GetIsTruncated())
            return keys;

        request.SetMarker(outcome.GetResult().GetNextMarker());
    }

    ONE_METRIC_TIMERCTX_STOP(timer, keys.size());

    LOG_DBG(1) << "Got object list at prefix " << prefix << ": "
               << LOG_VEC(keys);

    return keys;
}

} // namespace helpers
} // namespace one
