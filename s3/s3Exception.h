/**
 * @file s3Exception.h
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <Poco/Net/NetException.h>
#include <Poco/Net/HTTPResponse.h>
#include <drogon/HttpResponse.h>
#include <drogon/HttpTypes.h>
#include <fmt/format.h>

#include <stdexcept>

namespace one {
namespace s3 {
namespace error {

class S3Exception : public std::runtime_error {
public:
    S3Exception(const drogon::HttpStatusCode statusCode, const char *statusName,
        const char *message, std::string bucketName, std::string resourceName,
        std::string requestId)
        : std::runtime_error{message}
        , m_statusCode{statusCode}
        , m_statusName{statusName}
        , m_bucketName{std::move(bucketName)}
        , m_resourceName{std::move(resourceName)}
        , m_requestId{requestId}
    {
    }

    static void raiseFromPocoNetException(const Poco::Net::NetException &e,
        const std::string &bucket, const std::string &path,
        const std::string &requestId);

    static void raiseFromPocoHTTPException(const Poco::Net::HTTPException &e,
        const std::string &bucket, const std::string &path,
        const std::string &requestId);

    drogon::HttpStatusCode statusCode() const { return m_statusCode; }

    const char *statusName() const { return m_statusName; }

    const std::string &bucketName() const { return m_bucketName; }

    const std::string &resourceName() const { return m_resourceName; }

    std::string requestId() const { return m_requestId; }

    void fillResponse(std::shared_ptr<drogon::HttpResponse> &response) const
    {
        std::string body = fmt::format(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            "<Error>"
            "<Code>{}</Code>"
            "<Message>{}</Message>"
            "<BucketName>{}</BucketName>"
            "<Resource>/{}</Resource>"
            "<RequestId>{}</RequestId>"
            "<HostId>e225541b-7a4b-409c-b254-880c6b7c52c7</HostId>"
            "</Error>",
            statusName(), what(), bucketName(), resourceName(), requestId());

        response->setStatusCode(statusCode());
        response->addHeader("x-amz-request-id", requestId());
        response->addHeader("content-length", std::to_string(body.size()));
        response->setContentTypeString("application/xml");

        response->setBody(std::move(body));
    }

private:
    const drogon::HttpStatusCode m_statusCode;

    const char *m_statusName;
    const std::string m_bucketName;
    const std::string m_resourceName;
    const std::string m_requestId;
};

#define S3_EXCEPTION(NAME, CODE, MESSAGE)                                      \
    class NAME : public S3Exception {                                          \
    public:                                                                    \
        NAME(std::string bucketName, std::string resourceName,                 \
            std::string requestId)                                             \
            : S3Exception(drogon::HttpStatusCode::CODE, #NAME, MESSAGE,        \
                  std::move(bucketName), std::move(resourceName),              \
                  std::move(requestId))                                        \
        {                                                                      \
        }                                                                      \
    };

S3_EXCEPTION(
    InternalServerError, k500InternalServerError, "Internal server error.")
S3_EXCEPTION(AccessDenied, k403Forbidden, "Forbidden")
S3_EXCEPTION(BadDigest, k400BadRequest, "Bad Request")
S3_EXCEPTION(BucketAlreadyExists, k403Forbidden,
    "The requested bucket name is not available. The bucket namespace is "
    "shared by all users of the system. Please select a different name and try "
    "again.")
S3_EXCEPTION(BucketAlreadyOwnedByYou, k409Conflict,
    "Your previous request to create the named bucket succeeded and you "
    "already own it.")
S3_EXCEPTION(
    BucketNotEmpty, k409Conflict, "The bucket you tried to delete is not empty")
S3_EXCEPTION(EntityTooLarge, k400BadRequest,
    "Your proposed upload exceeds the maximum allowed object size.")
S3_EXCEPTION(EntityTooSmall, k400BadRequest,
    "Your proposed upload is smaller than the minimum allowed object size. "
    "Each part must be at least 5 MB in size, except the last part.")
S3_EXCEPTION(InvalidAccessKeyId, k403Forbidden, "Forbidden")
S3_EXCEPTION(InvalidArgument, k400BadRequest, "Bad Request")
S3_EXCEPTION(
    InvalidBucketName, k400BadRequest, "The specified bucket is not valid.")
S3_EXCEPTION(InvalidCORSOrigin, k400BadRequest,
    "Insufficient information. Origin request header needed.")
S3_EXCEPTION(InvalidCORSMethod, k400BadRequest,
    "The specified Access-Control-Request-Method is not valid.")
S3_EXCEPTION(InvalidDigest, k400BadRequest, "Bad Request")
S3_EXCEPTION(InvalidLocationConstraint, k400BadRequest,
    "The specified location constraint is not valid. For more information "
    "about Regions, see How to Select a Region for Your Buckets.")
S3_EXCEPTION(InvalidRange, k416RequestedRangeNotSatisfiable,
    "The requested range is not satisfiable")
S3_EXCEPTION(InvalidPart, k400BadRequest,
    "One or more of the specified parts could not be found. The part may not "
    "have been uploaded, or the specified entity tag may not match the part's "
    "entity tag.")
S3_EXCEPTION(InvalidRequest, k400BadRequest, "Bad Request")
S3_EXCEPTION(MalformedXML, k400BadRequest,
    "The XML you provided was not well-formed or did not validate against our "
    "published schema.")
S3_EXCEPTION(MessageLengthExceeded, k400BadRequest, "Your request was too big.")
S3_EXCEPTION(MethodNotAllowed, k400BadRequest, "Method Not Allowed.")
S3_EXCEPTION(MissingContentLength, k411LengthRequired, "Length Required.")
S3_EXCEPTION(NoSuchBucket, k404NotFound, "The specified bucket does not exist")
S3_EXCEPTION(NoSuchKey, k404NotFound, "The specified key does not exist.")
S3_EXCEPTION(NoSuchPolicy, k404NotFound,
    "The specified bucket does not have a bucket policy.")
S3_EXCEPTION(NoSuchUpload, k404NotFound, "Not Found")
S3_EXCEPTION(NotImplemented, k501NotImplemented,
    "A header you provided implies functionality that is not implemented.")
S3_EXCEPTION(PreconditionFailed, k412PreconditionFailed,
    "At least one of the preconditions you specified did not hold.")
S3_EXCEPTION(RequestTimeTooSkewed, k403Forbidden, "Forbidden")
S3_EXCEPTION(RequestTimeout, k400BadRequest, "Bad Request")
S3_EXCEPTION(SignatureDoesNotMatch, k403Forbidden, "Forbidden")
S3_EXCEPTION(XAMZContentSHA256Mismatch, k400BadRequest,
    "The provided 'x-amz-content-sha256' header does not match what was "
    "computed.")

} // namespace error
} // namespace s3
} // namespace one