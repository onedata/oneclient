/**
 * @file s3Exception.cc
 * @author Bartek Kryza
 * @copyright (C) 2022-present Onedata.org
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Exception.h"

namespace one {
namespace s3 {
namespace error {

void S3Exception::raiseFromPocoNetException(const Poco::Net::NetException &/*e*/,
    const std::string &bucket, const std::string &path,
    const std::string &requestId)
{
    throw one::s3::error::InternalServerError(bucket, path, requestId);
}

void S3Exception::raiseFromPocoHTTPException(const Poco::Net::HTTPException &e,
    const std::string &bucket, const std::string &path,
    const std::string &requestId)
{
    switch (e.code()) {
        case Poco::Net::HTTPResponse::HTTP_FORBIDDEN:
        case Poco::Net::HTTPResponse::HTTP_UNAUTHORIZED:
            throw one::s3::error::AccessDenied(bucket, path, requestId);
        case Poco::Net::HTTPResponse::HTTP_CONFLICT:
            throw one::s3::error::BucketAlreadyOwnedByYou(
                bucket, path, requestId);
        default:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
    }
}

} // namespace error
} // namespace s3
} // namespace one