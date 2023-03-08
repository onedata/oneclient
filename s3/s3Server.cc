/**
 * @file s3server.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Server.h"

#include "events/types/fileWritten.h"
#include "monitoring/monitoring.h"
#include "onepanelRestClient.h"
#include "onezoneRestClient.h"
#include "s3Exception.h"
#include "serialization.h"
#include "util/md5.h"
#include "util/mime.h"
#include "version.h"

#include <Poco/Net/NetException.h>
#include <RangeParser.h>
#include <aws/core/utils/xml/XmlSerializer.h>
#include <aws/s3/model/CompleteMultipartUploadRequest.h>
#include <aws/s3/model/CreateMultipartUploadResult.h>
#include <aws/s3/model/Delete.h>
#include <aws/s3/model/DeleteObjectsResult.h>
#include <aws/s3/model/GetObjectResult.h>
#include <aws/s3/model/ListBucketsResult.h>
#include <aws/s3/model/ListPartsResult.h>
#include <boost/preprocessor/facilities/overload.hpp>
#include <folly/Optional.h>
#include <folly/String.h>
#include <folly/io/IOBufQueue.h>

// This macro controls whether Drogon requests wait synchronously
// for s3Logic futures completion
#define FUTURE_GET_ASYNC
#define FUTURE_GET_SYNC .get()
#define FUTURE_GET() FUTURE_GET_ASYNC

namespace one {
namespace s3 {

using namespace drogon;
namespace {

template <typename T = std::string>
folly::Optional<T> getParameter(
    const HttpRequestPtr &req, const std::string &name)
{
    if (req->getParameters().find(name) == req->getParameters().end())
        return {};

    return {req->getParameter(name)};
}

template <>
folly::Optional<size_t> getParameter(
    const HttpRequestPtr &req, const std::string &name)
{
    if (req->getParameters().find(name) == req->getParameters().end())
        return {};

    return {std::stoull(req->getParameter(name))};
}

} // namespace

#define LOG_REQUEST_5(op, bucket, requestId, req, other)                       \
    LOG_DBG(1) << fmt::format("ones3 [{}] {} {} {}{}{}", requestId,            \
        Poco::DateTimeFormatter::format(                                       \
            Poco::Timestamp{}, Poco::DateTimeFormat::ISO8601_FORMAT),          \
        op, (req)->getPath(),                                                  \
        (req)->getQuery().empty() ? ""                                         \
                                  : fmt::format("?{}", (req)->getQuery()),     \
        (other).empty() ? "" : fmt::format(" ({})", (other)));

#define LOG_REQUEST_4(op, bucket, requestId, req)                              \
    LOG_REQUEST_5(op, bucket, requestId, req, std::string{""})

#define LOG_REQUEST(...)                                                       \
    BOOST_PP_OVERLOAD(LOG_REQUEST_, __VA_ARGS__)(__VA_ARGS__)

#define LOG_REQUEST_ERROR(REQUEST_ID_, MESSAGE_, REASON_)                      \
    LOG_DBG(1) << fmt::format(                                                 \
        "ones3 [{}] ERROR: {}: {}", REQUEST_ID_, MESSAGE_, REASON_);

bool S3Server::bucketNameCached(const std::string &name) const
{
    return m_bucketNameCache.find(name) != m_bucketNameCache.end();
}

std::string S3Server::getCachedBucketId(const std::string &name) const
{
    return m_bucketNameCache.at(name);
}

void S3Server::cacheBucketName(
    const std::string &name, const std::string &id) const
{
    LOG_DBG(3) << "Caching bucket " << name << " --> " << id;
    m_bucketNameCache.emplace(name, id);
}

std::string S3Server::getRequestId() const
{
    std::lock_guard<std::mutex> lockGuard(m_uuidGeneratorMutex);
    return boost::lexical_cast<std::string>(m_uuidGenerator());
}

std::unique_ptr<S3Authorization> S3Authorization::fromHttpRequest(
    const HttpRequestPtr &req)
{
    const auto &authorizationHeader = req->getHeader("Authorization");

    if (authorizationHeader.empty()) {
        // authorization can be also in the query directly sometimes
        if (req->parameters().find("X-Amz-Algorithm") !=
                req->parameters().end() &&
            req->getParameter("X-Amz-Algorithm") == "AWS4-HMAC-SHA256") {

            auto auth = std::make_unique<S3AuthorizationV4>();
            auth->algorithm = "AWS4-HMAC-SHA256";

            if (req->parameters().find("X-Amz-Credential") !=
                req->parameters().end())
                auth->parseCredential(req->getParameter("X-Amz-Credential"));
            else
                return std::make_unique<S3AuthorizationInvalid>();

            if (req->parameters().find("X-Amz-Signature") !=
                req->parameters().end())
                auth->signature = req->getParameter("X-Amz-Signature");
            else
                return std::make_unique<S3AuthorizationInvalid>();

            if (req->parameters().find("X-Amz-SignedHeaders") !=
                req->parameters().end())
                folly::split(";", req->getParameter("X-Amz-SignedHeaders"),
                    auth->signedHeaders, false);
            else
                return std::make_unique<S3AuthorizationInvalid>();

            return auth;
        }

        return std::make_unique<S3AuthorizationNone>();
    }

    if (authorizationHeader.find("AWS4-HMAC-SHA256") == 0) {
        // This is a V4 request

        auto auth = std::make_unique<S3AuthorizationV4>();
        auth->algorithm = "AWS4-HMAC-SHA256";

        auto credentialsIdx = authorizationHeader.find("Credential=");
        credentialsIdx += strlen("Credential=");
        std::string credential = authorizationHeader.substr(
            credentialsIdx, authorizationHeader.find(',') - credentialsIdx);
        auth->parseCredential(credential);

        auto signedHeadersIdx = authorizationHeader.find("SignedHeaders=");
        signedHeadersIdx += strlen("SignedHeaders=");
        auto signedHeaders = authorizationHeader.substr(signedHeadersIdx,
            authorizationHeader.find(',', signedHeadersIdx) - signedHeadersIdx);
        folly::split(';', signedHeaders, auth->signedHeaders, false);

        auto signatureIdx = authorizationHeader.find("Signature=");
        signatureIdx += strlen("Signature=");
        auth->signature = authorizationHeader.substr(signatureIdx);

        return auth;
    }

    return std::make_unique<S3AuthorizationNone>();
}

void S3AuthorizationV4::parseCredential(const std::string &credential)
{
    std::vector<std::string> credentialTokens;
    folly::split("/", credential, credentialTokens, false);

    accessKeyId = credentialTokens[0];
    date = credentialTokens[1];
    region = credentialTokens[2];
    service = credentialTokens[3];
}

void S3Server::listBuckets(
    const HttpRequestPtr &req, HttpResponseCallback &&callback)
{
    const auto requestId = getRequestId();

    auto auth = S3Authorization::fromHttpRequest(req);

    std::string token = auth->getToken();

    LOG_REQUEST("LIST_BUCKETS", "", requestId, req);

    ONE_METRIC_COUNTER_INC("comp.ones3.mod.s3server.list_buckets");

    m_logicCache->get(token)
        .thenTry([](auto &&s3) {
            s3.throwIfFailed();
            return s3.value()->listBuckets();
        })
        .thenTry([callback](auto &&buckets) {
            auto response = HttpResponse::newHttpResponse();
            response->setBody(
                serialize<Aws::S3::Model::ListBucketsResult>(buckets.value()));
            callback(response);
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [callback](auto &&e) mutable {
                auto response = HttpResponse::newHttpResponse();
                e.fillResponse(response);
                callback(response);
            })
        .thenError(
            folly::tag_t<std::exception>{}, [callback](auto && /*e*/) mutable {
                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

// EXAMPLE HEADERS
// 'User-Agent': b'aws-cli/1.22.34 Python/3.10.4 Linux/5.15.14-051514-generic
// botocore/1.23.34' 'X-Amz-Date': b'20220902T132615Z' 'X-Amz-Content-SHA256':
// b'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
// 'Authorization': b'AWS4-HMAC-SHA256
// Credential=MDAzM2xvY2F00aW9uIGRldi1vbmV6b25lLmRlZmF1bHQuc3ZjLmNsdXN00ZXIubG9jYWwKMDA2YmlkZW500aWZpZXIgMi9ubWQvdXNyLTljMjU00OTkyMmZiMzhmMGJiYTYyNGJiNTU00NDk3M2Y5Y2hiMzhmL2FjdC9lOTBjOGY5YzI3NzIxYzJkOGJjYjFhMzMwMmM00ODk5NmNoN2IzOAowMDFlY2lkIGludGVyZmFjZSA9IG9uZWNsaWVudAowMDJmc2lnbmF00dXJlIPItGH7wQJgbXGFMP2DHz14YMlamjaoYpl003avAb5VjmCg/20220902/us-east-1/s3/aws4_request,
// SignedHeaders=host;x-amz-content-sha256;x-amz-date
// Signature=25b80f3b16e18b082b5425629efa215f14c47a38926f9683e4ba673a8f63924a'
// 'Content-Length': '0'

void S3Server::putBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("CREATE_BUCKET", bucket, requestId, req);

    auto response = HttpResponse::newHttpResponse();

    try {
        auto auth = S3Authorization::fromHttpRequest(req);

        auto onezoneHost = m_options->getOnezoneHost().value();

        one::rest::onezone::OnezoneClient onezoneClient{onezoneHost};
        one::rest::onepanel::OnepanelClient onepanelClient{
            m_options->getProviderHost().value()};

        if (bucketNameCached(bucket))
            throw one::s3::error::BucketAlreadyOwnedByYou(
                bucket, bucket, requestId);

        try {
            // List spaces to check if that space already exists (regardless of
            // its seed)
            for (const auto &spaceId :
                onezoneClient.listUserSpaces(auth->getToken())) {
                if (onezoneClient.getUserSpace(auth->getToken(), spaceId)
                        .name == bucket) {
                    if (onepanelClient.ensureSpaceIsSupported(
                            m_options->getAccessToken().value(), spaceId))
                        throw one::s3::error::BucketAlreadyOwnedByYou(
                            bucket, bucket, requestId);
                }
            }

            // Create the new space
            std::string spaceId;
            try {
                spaceId = onezoneClient.createSpace(auth->getToken(), bucket);
            }
            catch (Poco::Net::HTTPException &e) {
                if (e.code() == Poco::Net::HTTPResponse::HTTP_CONFLICT) {
                    if (onepanelClient.ensureSpaceIsSupported(
                            m_options->getAccessToken().value(), spaceId))
                        throw one::s3::error::BucketAlreadyOwnedByYou(
                            bucket, bucket, requestId);
                }
                throw e;
            }

            // Add new space to admin group if specified on the command line
            // TODO: ...

            // Add storage support for the new space
            auto spaceSupportToken = onezoneClient.createSpaceSupportToken(
                auth->getToken(), spaceId);

            onepanelClient.supportSpace(m_options->getAccessToken().value(),
                spaceSupportToken,
                m_options->getOneS3SupportStorageId().value(),
                m_options->getOneS3SupportStorageSize());

            //
            // Wait for space to be visible
            //
            if (onepanelClient.ensureSpaceIsSupported(
                    m_options->getAccessToken().value(), spaceId)) {
                // Why do I have to do this? Shouldn't ensureSpaceIsSupported()
                // be enough
                const int kEnsureSpaceSupportRetryCount = 100;
                const int kEnsureSpaceSupportDelayMS = 100;
                auto retries = kEnsureSpaceSupportRetryCount;
                while (retries-- > 0) {
                    auto buckets = m_logicCache->get(auth->getToken())
                                       .delayed(std::chrono::milliseconds(
                                           kEnsureSpaceSupportDelayMS))
                                       .thenTry([](auto &&s3) {
                                           return s3.value()->listBuckets();
                                       })
                                       .get();

                    for (const auto &listedBucket : buckets.GetBuckets()) {
                        if (listedBucket.GetName() == bucket) {
                            cacheBucketName(bucket, spaceId);
                            response->addHeader("Location", "/" + bucket);
                            callback(response);
                            return;
                        }
                    }
                }
            }

            ONE_METRIC_COUNTER_INC(toMetricName("put_bucket", bucket));

            throw one::s3::error::InternalServerError(
                bucket, bucket, requestId);
        }
        catch (Poco::Net::HTTPException &e) {
            one::s3::error::S3Exception::raiseFromPocoHTTPException(
                e, bucket, bucket, requestId);
        }
    }
    catch (one::s3::error::S3Exception &e) {
        LOG_REQUEST_ERROR(requestId, "Failed to create bucket", e.what());
        e.fillResponse(response);
        callback(response);
    }
    catch (std::exception &e) {
        LOG_REQUEST_ERROR(requestId, "Failed to create bucket", e.what());
        response->setStatusCode(drogon::k500InternalServerError);
        callback(response);
    }
}

bool S3Server::ensureSpaceIsSupported(const std::string &bucket,
    const HttpResponseCallback &callback, const std::string &requestId,
    const std::string &token) const
{
    if (bucketNameCached(bucket))
        return true;

    auto response = HttpResponse::newHttpResponse();

    one::rest::onepanel::OnepanelClient onepanelClient{
        m_options->getProviderHost().value()};
    one::rest::onezone::OnezoneClient onezoneClient{
        m_options->getOnezoneHost().value()};

    try {
        try {
            // Get the space id
            std::string spaceId;
            for (const auto &space : onezoneClient.listUserSpaces(token)) {
                if (onezoneClient.getUserSpace(token, space).name == bucket)
                    spaceId = space;
            }

            if (spaceId.empty()) {
                throw one::s3::error::NoSuchBucket(bucket, bucket, requestId);
            }

            if (onepanelClient.ensureSpaceIsSupported(
                    m_options->getAccessToken().value(), spaceId)) {

                // Why do I have to do this? Shouldn't ensureSpaceIsSupported()
                // be enough
                const int kEnsureSpaceSupportRetryCount = 100;
                const int kEnsureSpaceSupportDelayMS = 100;

                auto retries = kEnsureSpaceSupportRetryCount;
                while (retries-- > 0) {
                    auto buckets = m_logicCache->get(token)
                                       .delayed(std::chrono::milliseconds(
                                           kEnsureSpaceSupportDelayMS))
                                       .thenTry([](auto &&s3) {
                                           return s3.value()->listBuckets();
                                       })
                                       .get();

                    for (const auto &listedBucket : buckets.GetBuckets()) {
                        if (listedBucket.GetName() == bucket) {
                            cacheBucketName(bucket, spaceId);
                            return true;
                        }
                    }
                }
            }

            throw one::s3::error::NoSuchBucket(bucket, bucket, requestId);
        }
        catch (Poco::Net::HTTPException &e) {
            one::s3::error::S3Exception::raiseFromPocoHTTPException(
                e, bucket, bucket, requestId);
        }
        catch (Poco::Net::NetException &e) {
            one::s3::error::S3Exception::raiseFromPocoNetException(
                e, bucket, bucket, requestId);
        }
    }
    catch (one::s3::error::S3Exception &e) {
        LOG_REQUEST_ERROR(
            requestId, "Request failed to create bucket", e.what());
        e.fillResponse(response);
        callback(response);
    }

    return false;
}

void S3Server::headBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    const auto requestId = getRequestId();

    auto auth = S3Authorization::fromHttpRequest(req);

    LOG_REQUEST("HEAD_BUCKET", bucket, requestId, req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    ONE_METRIC_COUNTER_INC(toMetricName("head_bucket", bucket));

    m_logicCache->get(auth->getToken())
        .thenTry(
            [bucket](auto &&s3) { return s3.value()->getBucketAttr(bucket); })
        .thenValue([callback](messages::fuse::FileAttr && /*bucketAttr*/) {
            auto response = HttpResponse::newHttpResponse();
            callback(response);
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [callback](auto &&e) mutable {
                auto response = HttpResponse::newHttpResponse();
                e.fillResponse(response);
                callback(response);
            })
        .thenError(
            folly::tag_t<std::exception>{}, [callback](auto && /*e*/) mutable {
                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::deleteBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("DELETE_BUCKET", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    auto onezoneHost = m_options->getOnezoneHost().value();

    one::rest::onezone::OnezoneClient onezoneClient{onezoneHost};

    auto response = HttpResponse::newHttpResponse();

    ONE_METRIC_COUNTER_INC(toMetricName("delete_bucket", bucket));

    try {
        folly::Optional<std::string> spaceIdToDelete;

        // Check if space exists
        auto userSpaceIds = onezoneClient.listUserSpaces(auth->getToken());
        for (const auto &spaceId : userSpaceIds) {
            auto userSpace =
                onezoneClient.getUserSpace(auth->getToken(), spaceId);
            if (userSpace.name == bucket) {
                spaceIdToDelete = spaceId;
            }
        }

        if (!spaceIdToDelete)
            throw one::s3::error::NoSuchBucket(bucket, bucket, requestId);

        bool isEmpty =
            m_logicCache->get(auth->getToken())
                .thenValue([&bucket](std::shared_ptr<S3Logic> &&s3) {
                    return s3->readDirV2Recursive(bucket, "", {}, 1, false)
                        .thenValue(
                            [](Aws::S3::Model::ListObjectsV2Result &&result) {
                                return result.GetKeyCount() == 0;
                            });
                })
                .get();

        if (!isEmpty)
            throw one::s3::error::BucketNotEmpty(bucket, bucket, requestId);

        onezoneClient.deleteSpace(auth->getToken(), spaceIdToDelete.value());

        constexpr auto kRetryCount{120};
        constexpr auto kRetryDelay{500};
        auto retries = kRetryCount;

        while (retries-- > 0) {
            bool bucketStillListing{false};

            auto buckets = m_logicCache->get(auth->getToken())
                               .delayed(std::chrono::milliseconds(kRetryDelay))
                               .thenValue([](std::shared_ptr<S3Logic> &&s3) {
                                   return s3->listBuckets();
                               })
                               .get();

            for (const auto &listedBucket : buckets.GetBuckets()) {
                if (listedBucket.GetName() == bucket) {
                    bucketStillListing = true;
                }
            }

            if (!bucketStillListing)
                break;
        }

        if (retries <= 0)
            throw one::s3::error::InternalServerError(
                bucket, bucket, requestId);

        response->setStatusCode(HttpStatusCode::k204NoContent);
    }
    catch (one::s3::error::S3Exception &e) {
        LOG_REQUEST_ERROR(requestId, "Delete bucket failed due to", e.what());
        e.fillResponse(response);
    }

    callback(response);
}

void S3Server::getLocationConstraint(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    const auto requestId = getRequestId();

    auto auth = S3Authorization::fromHttpRequest(req);

    LOG_REQUEST("GET_LOCATION", bucket, requestId, req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    m_logicCache->get(auth->getToken())
        .thenTry(
            [bucket](auto &&s3) { return s3.value()->getBucketAttr(bucket); })
        .thenTry([callback](auto &&bucketAttr) {
            bucketAttr.value();

            auto response = HttpResponse::newHttpResponse();
            response->setBody(
                R"(<?xml version="1.0" encoding="UTF-8"?><LocationConstraint xmlns="http://s3.amazonaws.com/doc/2006-03-01/"></LocationConstraint>)");
            callback(response);
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [callback](auto &&e) mutable {
                auto response = HttpResponse::newHttpResponse();
                e.fillResponse(response);
                callback(response);
            })
        .thenError(
            folly::tag_t<std::exception>{}, [callback](auto && /*e*/) mutable {
                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::getVersioning(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("GET_VERSIONING", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    m_logicCache->get(auth->getToken())
        .thenTry(
            [&bucket](auto &&s3) { return s3.value()->getBucketAttr(bucket); })
        .thenTry([callback](auto &&bucketAttr) {
            bucketAttr.value();

            auto response = HttpResponse::newHttpResponse();
            response->setBody(
                R"(<VersioningConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/"/>)");

            callback(response);
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [callback](auto &&e) mutable {
                auto response = HttpResponse::newHttpResponse();
                e.fillResponse(response);
                callback(response);
            })
        .thenError(
            folly::tag_t<std::exception>{}, [callback](auto && /*e*/) mutable {
                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::headObject(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("HEAD_OBJECT", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    ONE_METRIC_COUNTER_INC(toMetricName("head_object", bucket));

    m_logicCache->get(auth->getToken())
        .thenTry([bucket, path, requestId](auto &&s3) {
            return s3.value()->headObject(bucket, path, requestId);
        })
        .thenTry([callback, path, requestId](auto &&headResult) {
            auto response = HttpResponse::newHttpResponse();
            response->addHeader("content-length",
                std::to_string(headResult.value().GetContentLength()));
            if (!path.empty() && path.back() == '/')
                // This is required for handling empty directories
                response->setContentTypeString("application/octet-stream");
            else
                response->setContentTypeString(
                    headResult.value().GetContentType());
            response->addHeader("etag", headResult.value().GetETag());
            response->addHeader("accept-ranges", "bytes");
            response->addHeader("last-modified",
                headResult.value().GetLastModified().ToGmtString(
                    Aws::Utils::DateFormat::RFC822));
            response->addHeader("x-amz-request-id", requestId);

            callback(response);
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Head object failed due to", e.what());

                auto response = HttpResponse::newHttpResponse();
                e.fillResponse(response);
                callback(response);
            })
        .thenError(folly::tag_t<std::exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Head object failed due to", e.what());

                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::getObject(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();

    auto auth = S3Authorization::fromHttpRequest(req);

    if (req->method() == drogon::Head) {
        headObject(req, std::move(callback), bucket, path);
        return;
    }

    if (path.empty()) {
        getBucket(req, std::move(callback), bucket);
        return;
    }

    if (req->getParameters().find("uploadId") != req->getParameters().end()) {
        listMultipartUploadParts(req, std::move(callback), bucket, path);
        return;
    }

    folly::Optional<folly::fbstring> rangeHeader;
    if (req->getHeaders().find("range") != req->getHeaders().end()) {
        rangeHeader = req->getHeaders().at("range");
    }

    LOG_REQUEST("GET_OBJECT", bucket, requestId, req,
        fmt::format("{}", (rangeHeader.hasValue() ? rangeHeader.value() : "")));

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    auto timer = ONE_METRIC_TIMERCTX_CREATE(toMetricName("get_object", bucket));

    m_logicCache->get(auth->getToken())
        .thenValue([callback, bucket, path, rangeHeader, requestId,
                       timer = std::move(timer)](
                       std::shared_ptr<S3Logic> &&s3) mutable {
            // Extract range header if exist
            return s3
                ->getObject(bucket, path, requestId, rangeHeader,
                    [timer](size_t readBytes) {
                        ONE_METRIC_TIMERCTX_STOP(timer, readBytes);
                    })
                .thenValue([callback, path, timer = std::move(timer)](
                               auto &&args) mutable {
                    auto &headResult = args.first;
                    auto streamReader = args.second;

                    auto response =
                        HttpResponse::newStreamResponse(streamReader, path,
                            CT_NONE, headResult.GetContentType());

                    response->setContentTypeString(headResult.GetContentType());

                    response->addHeader("content-length",
                        std::to_string(headResult.GetContentLength()));
                    response->addHeader("etag", headResult.GetETag());
                    response->addHeader("accept-ranges", "bytes");
                    response->addHeader("last-modified",
                        headResult.GetLastModified().ToGmtString(
                            Aws::Utils::DateFormat::RFC822));

                    callback(response);
                });
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Get object failed due to", e.what());

                auto response = HttpResponse::newHttpResponse();
                e.fillResponse(response);
                callback(response);
            })
        .thenError(folly::tag_t<std::exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Get object failed due to", e.what());

                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::postBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    if (req->getQuery().find("delete") != std::string::npos) {
        deleteObjects(req, std::move(callback), bucket);
    }
    else {
        auto resp = HttpResponse::newHttpResponse();
        resp->setBody("Invalid request.");
        resp->setStatusCode(k400BadRequest);
        callback(resp);
    }
}

void S3Server::postObject(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    auto auth = S3Authorization::fromHttpRequest(req);

    if (req->getQuery().find("uploads") != std::string::npos) {
        createMultipartUpload(req, std::move(callback), bucket, path);
    }
    else if (req->getParameters().find("uploadId") !=
        req->getParameters().end()) {
        completeMultipartUpload(req, std::move(callback), bucket, path);
    }
    else if (path.empty() &&
        req->getQuery().find("delete=") != std::string::npos) {
        deleteObjects(req, std::move(callback), bucket);
    }
    else {
        auto resp = HttpResponse::newHttpResponse();
        resp->setBody("Invalid request.");
        resp->setStatusCode(k400BadRequest);
        callback(resp);
    }
}

void S3Server::putObject(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    // Dispatch putObject request depending on query parameters
    const auto &queryParams = req->getParameters();
    if ((queryParams.find("uploadId") != queryParams.end()) &&
        (queryParams.find("partNumber") != queryParams.end())) {
        putMultipartPart(req, std::move(callback), bucket, path);
    }
    else {
        putCompleteObject(req, std::move(callback), bucket, path);
    }
}

void S3Server::putMultipartPart(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();
    const auto uploadId = req->getParameter("uploadId");
    const auto partNumber = std::stoul(req->getParameter("partNumber"));

    LOG_REQUEST("PUT_MULTIPART_PART", bucket, requestId, req,
        fmt::format("{}, {}", uploadId, partNumber));

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    auto response = HttpResponse::newHttpResponse();

    auto timer = ONE_METRIC_TIMERCTX_CREATE(toMetricName("put_part", bucket));

    m_logicCache->get(auth->getToken())
        .thenTry([&req, uploadId, partNumber, response, callback, bucket, path,
                     requestId, timer](auto &&s3) {
            s3.throwIfFailed();

            const char *bodyData{nullptr};
            size_t bodyLength{0};
            std::string bodyMD5;

            MultiPartParser fileUpload;

            if (req->bodyLength() && req->bodyData() != nullptr) {
                bodyData = req->getBody().data();
                bodyLength = req->bodyLength();
            }
            else if (fileUpload.parse(req) != 0) {
                if (fileUpload.getFiles().size() != 1) {
                    throw one::s3::error::InvalidRequest(
                        bucket, path, requestId);
                }

                const auto &file = fileUpload.getFiles()[0];
                bodyData = file.fileData();
                bodyLength = file.fileLength();
                bodyMD5 = file.getMd5();
            }

            if (bodyMD5.empty())
                bodyMD5 = one::client::util::md5::md5(bodyData, bodyLength);

            std::shared_ptr<folly::IOBuf> buf{
                folly::IOBuf::copyBuffer(bodyData, bodyLength)};

            return s3.value()
                ->uploadMultipartPart(requestId, bucket, path, uploadId,
                    partNumber, bodyLength, bodyMD5, buf)
                .thenValue([bodyMD5, bodyLength, response, callback, timer](
                               auto && /*unit*/) {
                    LOG_DBG(2) << "File uploaded";

                    response->addHeader("etag", fmt::format("\"{}\"", bodyMD5));
                    response->addHeader("content-length", "0");

                    ONE_METRIC_TIMERCTX_STOP(timer, bodyLength);

                    callback(response);
                });
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [response, callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Put part failed due to", e.what());
                e.fillResponse(response);
                callback(response);
            })
        .thenError(folly::tag_t<std::exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Put part failed due to", e.what());
                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::putCompleteObject(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("PUT_OBJECT", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    auto response = HttpResponse::newHttpResponse();

    auto timer = ONE_METRIC_TIMERCTX_CREATE(toMetricName("put_object", bucket));

    m_logicCache->get(auth->getToken())
        .thenValue([&req, response, callback, bucket, path, requestId, timer](
                       std::shared_ptr<S3Logic> &&s3) {
            const char *bodyData{nullptr};
            size_t bodyLength{0};
            std::string bodyMD5;
            std::string bodyContentType;

            MultiPartParser fileUpload;

            if (req->bodyLength() > 0 && req->bodyData() != nullptr) {
                bodyData = req->getBody().data();
                bodyLength = req->bodyLength();
            }
            else if (fileUpload.parse(req) != 0) {
                if (fileUpload.getFiles().size() > 1) {
                    response->setBody(
                        "Only one file can be uploaded in one request");
                    response->setStatusCode(k403Forbidden);
                    callback(response);
                    return folly::makeFuture();
                }

                if (fileUpload.getFiles().empty()) {
                    bodyLength = 0;
                }
                else {
                    const auto &file = fileUpload.getFiles()[0];
                    bodyData = file.fileData();
                    bodyLength = file.fileLength();
                    bodyMD5 = file.getMd5();
                }
            }

            if (req->headers().find("x-amz-decoded-content-length") !=
                req->headers().end()) {
                bodyLength =
                    std::stoull(req->getHeader("x-amz-decoded-content-length"));
            }

            if (req->headers().find("content-type") != req->headers().end())
                bodyContentType = req->headers().at("content-type");
            else
                bodyContentType = one::client::util::mime::mimeFromPath(path);

            if (bodyMD5.empty())
                bodyMD5 = one::client::util::md5::md5(bodyData, bodyLength);

            std::shared_ptr<folly::IOBuf> buf{
                folly::IOBuf::copyBuffer(bodyData, bodyLength)};

            return s3
                ->uploadObject(requestId, bucket, path, bodyMD5,
                    bodyContentType, std::move(buf))
                .thenValue([callback, response, bodyMD5, timer](
                               auto &&written) {
                    LOG_DBG(2) << "File uploaded";

                    response->addHeader("etag", fmt::format("\"{}\"", bodyMD5));
                    response->addHeader("content-length", "0");

                    ONE_METRIC_TIMERCTX_STOP(timer, written);

                    callback(response);
                });
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [response, callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Put object failed due to", e.what());
                e.fillResponse(response);
                callback(response);
            })
        .thenError(folly::tag_t<std::exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Put object failed due to", e.what());
                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::deleteObject(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("DELETE_OBJECT", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    auto response = HttpResponse::newHttpResponse();

    ONE_METRIC_COUNTER_INC(toMetricName("delete_object", bucket));

    m_logicCache->get(auth->getToken())
        .thenValue([callback, &bucket, &path, requestId](
                       std::shared_ptr<S3Logic> &&s3) {
            return s3->deleteObject(requestId, bucket, path)
                .thenTry([callback](folly::Try<folly::Unit> &&value) {
                    if (value.hasException()) {
                        auto response = HttpResponse::newHttpResponse();
                        response->setStatusCode(HttpStatusCode::k404NotFound);
                        callback(response);
                        return;
                    }

                    auto response = HttpResponse::newHttpResponse();
                    response->setStatusCode(drogon::k204NoContent);
                    callback(response);
                });
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [response, callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Delete object failed due to", e.what());
                e.fillResponse(response);
                callback(response);
            })
        .thenError(folly::tag_t<std::exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Delete object failed due to", e.what());
                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::deleteObjects(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("DELETE_OBJECTS", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    auto response = HttpResponse::newHttpResponse();

    std::string body = req->getBody().to_string();
    auto requestXml = Aws::Utils::Xml::XmlDocument::CreateFromXmlString(body);
    auto deleteRequest = Aws::S3::Model::Delete{requestXml.GetRootElement()};

    ONE_METRIC_COUNTER_INC(toMetricName("delete_objects", bucket));

    m_logicCache->get(auth->getToken())
        .thenValue([deleteRequest = std::move(deleteRequest), bucket,
                       requestId](auto &&s3) {
            std::vector<folly::Future<
                std::pair<std::string, folly::Optional<std::string>>>>
                futs;

            for (const auto &object : deleteRequest.GetObjects()) {
                futs.emplace_back(
                    s3->deleteObject(requestId, bucket, object.GetKey())
                        .thenTry([key = object.GetKey()](auto &&result) {
                            if (result.hasException()) {
                                return std::make_pair(key,
                                    folly::Optional<std::string>(
                                        "AccessDenied"));
                            }

                            return std::make_pair(
                                key, folly::Optional<std::string>{});
                        }));
            }

            return folly::collectAll(futs);
        })
        .thenTry([callback](auto &&futs) {
            futs.throwIfFailed();

            Aws::S3::Model::DeleteObjectsResult result;

            for (const auto &fut : futs.value()) {
                if (!std::get<1>(fut.value()).hasValue()) {
                    Aws::S3::Model::DeletedObject deleted;
                    deleted.SetKey(std::get<0>(fut.value()));
                    result.AddDeleted(std::move(deleted));
                }
                else {
                    Aws::S3::Model::Error error;
                    error.SetKey(std::get<0>(fut.value()));
                    error.SetCode(std::get<1>(fut.value()).value());
                    error.SetMessage(std::get<1>(fut.value()).value());
                    result.AddErrors(std::move(error));
                }
            }

            auto response = HttpResponse::newHttpResponse();
            response->setBody(
                serialize<Aws::S3::Model::DeleteObjectsResult>(result));
            callback(response);
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Delete objects failed due to", e.what());
                auto response = HttpResponse::newHttpResponse();
                e.fillResponse(response);
                callback(response);
            })
        .thenError(folly::tag_t<std::exception>{},
            [callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "Delete objects failed due to", e.what());
                auto response = HttpResponse::newHttpResponse();
                response->setStatusCode(drogon::k500InternalServerError);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::getBucket(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    // Dispatch getBucket request depending on query parameters
    const auto &queryParams = req->getParameters();

    //
    // Perform head on the bucket
    //
    if (queryParams.find("location") != queryParams.end()) {
        getLocationConstraint(req, std::move(callback), bucket);
    }
    else if (queryParams.find("versioning") != queryParams.end()) {
        getVersioning(req, std::move(callback), bucket);
    }
    else {
        listObjects(req, std::move(callback), bucket);
    }
}

void S3Server::listObjects(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("LIST_OBJECTS", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    ONE_METRIC_COUNTER_INC(toMetricName("list_objects", bucket));

    folly::Optional<folly::fbstring> marker;
    auto delimiter = getParameter(req, "delimiter").value_or("");
    auto prefix = getParameter(req, "prefix").value_or("");
    auto listType = getParameter(req, "list-type");
    auto continuationToken = getParameter(req, "continuation-token");
    auto startAfter = getParameter(req, "start-after");
    int listVersion = 2;
    if (!listType || listType.value() == "1") {
        if (getParameter(req, "marker") &&
            !getParameter(req, "marker").value().empty()) {
            marker = getParameter(req, "marker").value();
        }
        listVersion = 1;
    }
    else if (listType.value() == "2") {
        if (continuationToken && !continuationToken.value().empty()) {
            marker = continuationToken.value();
        }
        else if (startAfter) {
            marker = startAfter.value();
        }
    }
    else {
        auto response = HttpResponse::newHttpResponse();
        response->setStatusCode(HttpStatusCode::k501NotImplemented);
        callback(response);
        return;
    }

    constexpr auto kMaxKeysDefault{1000};
    size_t maxKeys{kMaxKeysDefault};
    if (getParameter(req, "max-keys")) {
        maxKeys = std::min<size_t>(
            maxKeys, std::stoull(getParameter(req, "max-keys").value()));
    }

    auto tokenFuture = m_logicCache->get(auth->getToken());
    if (delimiter.empty()) {
        std::move(tokenFuture)
            .thenValue([listVersion, callback = std::move(callback), bucket,
                           prefix, marker,
                           maxKeys](std::shared_ptr<S3Logic> &&s3) mutable {
                if (listVersion == 2)
                    s3->readDirV2Recursive(bucket, prefix, marker, maxKeys)
                        .thenValue([callback](auto &&result) {
                            auto response = HttpResponse::newHttpResponse();
                            response->setContentTypeString("application/xml");
                            response->setBody(
                                serialize<Aws::S3::Model::ListObjectsV2Result>(
                                    result));
                            callback(response);
                        })
                        .get();
                else
                    s3->readDirRecursive(bucket, prefix, marker, maxKeys)
                        .thenValue([callback](auto &&result) {
                            auto response = HttpResponse::newHttpResponse();
                            response->setContentTypeString("application/xml");
                            response->setBody(
                                serialize<Aws::S3::Model::ListObjectsResult>(
                                    result));
                            callback(response);
                        })
                        .get();
            });
    }
    else {
        std::move(tokenFuture)
            .thenValue([listVersion, callback = std::move(callback), bucket,
                           prefix, marker, &delimiter,
                           maxKeys](std::shared_ptr<S3Logic> &&s3) mutable {
                if (listVersion == 2)
                    s3->readDirV2(bucket, prefix, marker, delimiter, maxKeys)
                        .thenTry([callback = std::move(callback)](
                                     auto &&result) {
                            auto response = HttpResponse::newHttpResponse();
                            response->setContentTypeString("application/xml");
                            response->setBody(
                                serialize<Aws::S3::Model::ListObjectsV2Result>(
                                    result.value()));
                            callback(response);
                        })
                        .get();
                else
                    s3->readDir(bucket, prefix, marker, delimiter, maxKeys)
                        .thenValue([callback = std::move(callback)](
                                       auto &&result) {
                            auto response = HttpResponse::newHttpResponse();
                            response->setContentTypeString("application/xml");
                            response->setBody(
                                serialize<Aws::S3::Model::ListObjectsResult>(
                                    result));
                            callback(response);
                        })
                        .get();
            });
    }
}

void S3Server::createMultipartUpload(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("CREATE_MULTIPART_UPLOAD", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    ONE_METRIC_COUNTER_INC(toMetricName("create_multipart_upload", bucket));

    auto response = HttpResponse::newHttpResponse();

    std::string contentType{"application/octet-stream"};

    if (req->headers().find("content-type") != req->headers().end())
        contentType = req->headers().at("content-type");

    m_logicCache->get(auth->getToken())
        .thenValue([requestId, callback = std::move(callback), response, bucket,
                       path,
                       contentType](std::shared_ptr<S3Logic> &&s3) mutable {
            return s3
                ->createMultipartUpload(bucket, path, requestId, contentType)
                .thenTry([response, callback = std::move(callback)](folly::Try<
                             Aws::S3::Model::CreateMultipartUploadResult>
                                 &&result) {
                    response->setContentTypeString("application/xml");

                    if (result.hasException()) {
                        response->setStatusCode(
                            HttpStatusCode::k500InternalServerError);
                        callback(response);
                        return;
                    }

                    response->setBody(
                        serialize<Aws::S3::Model::CreateMultipartUploadResult>(
                            result.value()));
                    callback(response);
                });
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [response, callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(requestId,
                    "Create multipart upload failed due to", e.what());
                e.fillResponse(response);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::abortMultipartUpload(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("ABORT_MULTIPART_UPLOAD", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    ONE_METRIC_COUNTER_INC(toMetricName("abort_multipart_upload", bucket));

    auto response = HttpResponse::newHttpResponse();

    m_logicCache->get(auth->getToken())
        .thenValue([response, callback = std::move(callback), bucket, path](
                       std::shared_ptr<S3Logic> &&s3) mutable {
            return s3->abortMultipartUpload(bucket, path)
                .thenValue([response, callback = std::move(callback)](
                               auto && /*result*/) {
                    response->setStatusCode(HttpStatusCode::k204NoContent);
                    callback(response);
                });
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [response, callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(requestId,
                    "Abort multipart upload failed due to", e.what());
                e.fillResponse(response);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::completeMultipartUpload(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();

    LOG_REQUEST("COMPLETE_MULTIPART_UPLOAD", bucket, requestId, req);

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    auto uploadId = req->getParameter("uploadId");
    auto response = HttpResponse::newHttpResponse();

    ONE_METRIC_COUNTER_INC(toMetricName("complete_multipart_upload", bucket));

    m_logicCache->get(auth->getToken())
        .thenValue([response, callback, requestId, path, bucket, uploadId](
                       std::shared_ptr<S3Logic> &&s3) {
            // Report upload completion to oneprovider
            return s3->completeMultipartUpload(
                requestId, bucket, path, uploadId);
        })
        .thenTry([response, callback](auto &&result) {
            result.throwIfFailed();

            response->setStatusCode(HttpStatusCode::k200OK);
            std::string body =
                serialize<Aws::S3::Model::CompleteMultipartUploadResult>(
                    result.value());
            response->setBody(std::move(body));

            callback(response);
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [response, callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(requestId,
                    "Complete multipart upload failed due to", e.what());
                e.fillResponse(response);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::listMultipartUploadParts(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
    const auto requestId = getRequestId();
    const auto uploadId = req->getParameter("uploadId");
    const auto maxParts = getParameter<size_t>(req, "max-parts").value_or(1000);
    const auto partNumberMarker =
        getParameter<size_t>(req, "part-number-marker");

    LOG_REQUEST("LIST_MULTIPART_UPLOAD_PARTS", bucket, requestId, req,
        fmt::format("{}, {}, {}", uploadId, maxParts,
            (partNumberMarker.hasValue()
                    ? std::to_string(partNumberMarker.value())
                    : "")));

    auto auth = S3Authorization::fromHttpRequest(req);

    if (!ensureSpaceIsSupported(
            bucket, callback, requestId, auth->getToken())) {
        return;
    }

    auto response = HttpResponse::newHttpResponse();

    ONE_METRIC_COUNTER_INC(toMetricName("list_multipart_uploads", bucket));

    m_logicCache->get(auth->getToken())
        .thenValue([response, callback = std::move(callback), requestId, path,
                       bucket, uploadId, maxParts, partNumberMarker](
                       std::shared_ptr<S3Logic> &&s3) mutable {
            return s3
                ->listMultipartUploadParts(
                    uploadId, bucket, path, maxParts, partNumberMarker)
                .thenValue(
                    [response, callback = std::move(callback)](auto &&result) {
                        response->setContentTypeString("application/xml");
                        response->setBody(
                            serialize<Aws::S3::Model::ListPartsResult>(result));
                        callback(response);
                    });
        })
        .thenError(folly::tag_t<one::s3::error::S3Exception>{},
            [response, callback, requestId](auto &&e) mutable {
                LOG_REQUEST_ERROR(
                    requestId, "List multipart upload failed due to", e.what());
                e.fillResponse(response);
                callback(response);
            }) FUTURE_GET();
}

void S3Server::listMultipartUploads(const HttpRequestPtr &req,
    HttpResponseCallback &&callback, const std::string &bucket,
    const std::string &path) const
{
}

void S3Server::readinessProbe(
    const HttpRequestPtr &req, HttpResponseCallback &&callback)
{
    auto response = HttpResponse::newHttpResponse();

    try {
        if (m_readinessProbeBasicAuth.has_value()) {
            if (req->getHeaders().find("authorization") ==
                req->getHeaders().end())
                throw one::s3::error::AccessDenied("", "", "");

            const auto &authorizationHeader = req->getHeader("Authorization");
            if (authorizationHeader.find("Basic ") != 0)
                throw one::s3::error::AccessDenied("", "", "");

            if (authorizationHeader == "Basic ")
                throw one::s3::error::AccessDenied("", "", "");

            const auto authorizationB64 =
                authorizationHeader.substr(strlen("Basic "));
            std::string authorizationDecoded;
            client::util::base64::base64_decode(
                authorizationB64, authorizationDecoded);

            if (authorizationDecoded != m_readinessProbeBasicAuth)
                throw one::s3::error::AccessDenied("", "", "");
        }

        Poco::JSON::Object body;
        body.set("version", ONECLIENT_VERSION);
        bool isOk{false};

        Poco::JSON::Array clients;

        isOk = m_logicCache->updateClientStatus(clients);

        body.set("clients", clients);
        body.set("isOk", isOk);

        auto bodyStr = rest::toString(body);

        response->setStatusCode(isOk
                ? drogon::HttpStatusCode::k200OK
                : drogon::HttpStatusCode::k500InternalServerError);
        response->addHeader("content-length", std::to_string(bodyStr.size()));
        response->setContentTypeString("application/json");
        response->setBody(std::move(bodyStr));

        callback(response);
    }
    catch (const one::s3::error::S3Exception &e) {
        e.fillResponse(response);
        callback(response);
    }
}

std::string S3Server::toMetricName(
    const std::string &op, const std::string &bucket) const
{
    return fmt::format(
        "comp.ones3.mod.s3server.{}.{}", op, getCachedBucketId(bucket));
}
} // namespace s3
} // namespace one