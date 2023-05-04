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

void S3Exception::raiseFromPocoNetException(
    const Poco::Net::NetException & /*e*/, const std::string &bucket,
    const std::string &path, const std::string &requestId)
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

void S3Exception::raiseFromSystemError(const std::system_error &e,
    const std::string &bucket, const std::string &path,
    const std::string &requestId)
{
    switch (e.code().value()) {
        case E2BIG:
        case EACCES:
            throw one::s3::error::AccessDenied(bucket, path, requestId);
        case EADDRINUSE:
        case EADDRNOTAVAIL:
        case EAFNOSUPPORT:
        case EAGAIN:
            throw one::s3::error::RequestTimeout(bucket, path, requestId);
        case EALREADY:
            throw one::s3::error::RequestTimeout(bucket, path, requestId);
        case EBADF:
            throw one::s3::error::AccessDenied(bucket, path, requestId);
        case EBADMSG:
            throw one::s3::error::InvalidRequest(bucket, path, requestId);
        case EBUSY:
            throw one::s3::error::RequestTimeout(bucket, path, requestId);
        case ECANCELED:
            throw one::s3::error::RequestTimeout(bucket, path, requestId);
        case ECHILD:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ECONNABORTED:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ECONNREFUSED:
            throw one::s3::error::AccessDenied(bucket, path, requestId);
        case ECONNRESET:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EDEADLK:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EDESTADDRREQ:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EDOM:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EDQUOT:
            throw one::s3::error::EntityTooLarge(bucket, path, requestId);
        case EEXIST:
            throw one::s3::error::BucketAlreadyExists(bucket, path, requestId);
        case EFAULT:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EFBIG:
            throw one::s3::error::EntityTooLarge(bucket, path, requestId);
        case EHOSTUNREACH:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EIDRM:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EILSEQ:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EINPROGRESS:
            throw one::s3::error::RequestTimeout(bucket, path, requestId);
        case ENOENT:
            throw one::s3::error::NoSuchKey(bucket, path, requestId);
        case EINTR:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EINVAL:
            throw one::s3::error::InvalidRequest{bucket, path, requestId};
        case EIO:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EISCONN:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EISDIR:
            throw one::s3::error::AccessDenied(bucket, path, requestId);
        case ELOOP:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EMFILE:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EMLINK:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EMSGSIZE:
            throw one::s3::error::EntityTooLarge(bucket, path, requestId);
        case EMULTIHOP:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENAMETOOLONG:
            throw one::s3::error::InvalidRequest(bucket, path, requestId);
        case ENETDOWN:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENETRESET:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENETUNREACH:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENFILE:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOBUFS:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENODATA:
            throw one::s3::error::NoSuchKey(bucket, path, requestId);
        case ENOEXEC:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOLCK:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOLINK:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOMEM:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOMSG:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOPROTOOPT:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOSR:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOSTR:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOSYS:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOTCONN:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOTDIR:
            throw one::s3::error::InvalidRequest(bucket, path, requestId);
        case ENOTEMPTY:
            throw one::s3::error::BucketNotEmpty(bucket, path, requestId);
        case ENOTRECOVERABLE:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOTSOCK:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOTSUP:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENOTTY:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ENXIO:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EOVERFLOW:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EOWNERDEAD:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EPERM:
            throw one::s3::error::AccessDenied(bucket, path, requestId);
        case EPIPE:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EPROTO:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EPROTONOSUPPORT:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case EPROTOTYPE:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ERANGE:
            throw one::s3::error::InvalidRange(bucket, path, requestId);
        case EROFS:
            throw one::s3::error::AccessDenied(bucket, path, requestId);
        case ESPIPE:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ESRCH:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ESTALE:
            throw one::s3::error::RequestTimeout(bucket, path, requestId);
        case ETIME:
            throw one::s3::error::InternalServerError(bucket, path, requestId);
        case ETIMEDOUT:
            throw one::s3::error::RequestTimeout(bucket, path, requestId);
        case ETXTBSY:
            throw one::s3::error::RequestTimeout(bucket, path, requestId);
        case EXDEV:
            throw one::s3::error::NoSuchUpload{bucket, path, requestId};
        case ENOSPC:
            throw one::s3::error::EntityTooLarge{bucket, path, requestId};
        default:
            throw one::s3::error::InternalServerError{bucket, path, requestId};
    }
}

} // namespace error
} // namespace s3
} // namespace one