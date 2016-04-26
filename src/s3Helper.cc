/**
 * @file s3Helper.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Helper.h"
#include "logging.h"

#include <glog/stl_logging.h>

#include <algorithm>
#include <ctime>
#include <functional>
#include <sstream>
#include <vector>

namespace one {
namespace helpers {

std::mutex S3Helper::s_mutex{};

const std::map<int, error_t> S3Helper::s_errorsTranslation = {
    {S3StatusOutOfMemory, makePosixError(std::errc::not_enough_memory)},
    {S3StatusInterrupted, makePosixError(std::errc::interrupted)},
    {S3StatusConnectionFailed, makePosixError(std::errc::connection_refused)},
    {S3StatusErrorAccessDenied, makePosixError(std::errc::permission_denied)},
    {S3StatusErrorInvalidArgument, makePosixError(std::errc::invalid_argument)},
    {S3StatusErrorNoSuchKey,
        makePosixError(std::errc::no_such_file_or_directory)},
    {S3StatusHttpErrorNotFound,
        makePosixError(std::errc::no_such_file_or_directory)},
    {S3StatusErrorNotImplemented,
        makePosixError(std::errc::function_not_supported)},
    {S3StatusErrorOperationAborted,
        makePosixError(std::errc::connection_aborted)},
    {S3StatusErrorRequestTimeout, makePosixError(std::errc::timed_out)},
    {S3StatusHttpErrorForbidden, makePosixError(std::errc::permission_denied)}};

S3Helper::S3Helper(std::unordered_map<std::string, std::string> args,
    asio::io_service &service)
    : m_service{service}
    , m_args{std::move(args)}
{
    std::lock_guard<std::mutex> guard{s_mutex};
    S3_initialize(nullptr, S3_INIT_ALL, nullptr);
}

S3Helper::~S3Helper() { S3_deinitialize(); }

CTXPtr S3Helper::createCTX(std::unordered_map<std::string, std::string> params)
{
    return std::make_shared<S3HelperCTX>(std::move(params), m_args);
}

void S3Helper::ash_unlink(
    CTXPtr rawCTX, const boost::filesystem::path &p, VoidCallback callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto fileId = p.string();

    asio::post(m_service, [
        this, ctx = std::move(ctx), fileId = std::move(fileId),
        callback = std::move(callback)
    ]() {
        try {
            sh_unlink(*ctx, fileId);
            callback(SUCCESS_CODE);
        }
        catch (const std::system_error &e) {
            callback(e.code());
        }
    });
}

void S3Helper::ash_read(CTXPtr rawCTX, const boost::filesystem::path &p,
    asio::mutable_buffer buf, off_t offset,
    GeneralCallback<asio::mutable_buffer> callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto fileId = p.string();

    asio::post(m_service, [
        =, ctx = std::move(ctx), buf = std::move(buf),
        fileId = std::move(fileId), callback = std::move(callback)
    ]() mutable {
        try {
            callback(
                sh_read(*ctx, fileId, std::move(buf), offset), SUCCESS_CODE);
        }
        catch (const std::system_error &e) {
            callback(asio::mutable_buffer{}, e.code());
        }
    });
}

void S3Helper::ash_write(CTXPtr rawCTX, const boost::filesystem::path &p,
    asio::const_buffer buf, off_t offset,

    GeneralCallback<std::size_t> callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto fileId = p.string();

    asio::post(m_service, [
        =, ctx = std::move(ctx), buf = std::move(buf),
        fileId = std::move(fileId), callback = std::move(callback)
    ]() mutable {
        try {
            callback(
                sh_write(*ctx, fileId, std::move(buf), offset), SUCCESS_CODE);
        }
        catch (const std::system_error &e) {
            callback(0, e.code());
        }
    });
}

void S3Helper::ash_truncate(CTXPtr rawCTX, const boost::filesystem::path &p,
    off_t size, VoidCallback callback)
{
    auto ctx = getCTX(std::move(rawCTX));
    auto fileId = p.string();

    asio::post(m_service, [
        =, ctx = std::move(ctx), fileId = std::move(fileId),
        callback = std::move(callback)
    ]() {
        try {
            sh_truncate(*ctx, fileId, size);
            callback(SUCCESS_CODE);
        }
        catch (const std::system_error &e) {
            callback(e.code());
        }
    });
}

void S3Helper::sh_unlink(const S3HelperCTX &ctx, const std::string &fileId)
{
    Operation data{"sh_unlink"};
    ResponseHandler<Operation> responseHandler;
    S3_delete_object(&ctx.bucketCTX, fileId.c_str(), nullptr, &responseHandler,
        static_cast<void *>(&data));
}

asio::mutable_buffer S3Helper::sh_read(const S3HelperCTX &ctx,
    const std::string &fileId, asio::mutable_buffer buf, off_t offset)
{
    ReadCallbackData data;
    data.buffer = std::move(buf);

    S3GetObjectHandler getObjectHandler;
    getObjectHandler.responseHandler = ResponseHandler<ReadCallbackData>{};
    getObjectHandler.getObjectDataCallback = [](
        int bufferSize, const char *buffer, void *callbackData) {
        auto dataPtr = static_cast<ReadCallbackData *>(callbackData);
        asio::buffer_copy(dataPtr->buffer + dataPtr->size,
            asio::const_buffer(buffer, bufferSize), bufferSize);
        dataPtr->size += bufferSize;
        return S3StatusOK;
    };

    S3_get_object(&ctx.bucketCTX, fileId.c_str(), nullptr, offset,
        asio::buffer_size(data.buffer), nullptr, &getObjectHandler,
        static_cast<void *>(&data));

    return asio::buffer(data.buffer, data.size);
}

std::size_t S3Helper::sh_write(const S3HelperCTX &ctx,
    const std::string &fileId, asio::const_buffer buf, off_t offset)
{
    std::size_t fileSize = 0;
    try {
        fileSize = sh_getFileSize(ctx, fileId);
    }
    catch (const std::system_error &e) {
        if (e.code() != makePosixError(std::errc::no_such_file_or_directory))
            throw;
    }
    return sh_write(ctx, fileId, std::move(buf), offset, fileSize);
}

std::size_t S3Helper::sh_write(const S3HelperCTX &ctx,
    const std::string &fileId, asio::const_buffer buf, off_t offset,
    std::size_t fileSize)
{
    WriteCallbackData data{fileId, ctx, *this};
    data.fileSize = fileSize;
    data.bufferOffset = offset;
    data.bufferSize = asio::buffer_size(buf);
    data.buffer = std::move(buf);

    auto tmpFileId = temporaryFileId(fileId);
    auto newFileSize =
        std::max(data.fileSize, data.bufferOffset + data.bufferSize);
    S3PutObjectHandler putObjectHandler;
    putObjectHandler.responseHandler = ResponseHandler<WriteCallbackData>{};
    putObjectHandler.putObjectDataCallback = [](
        int bufferSize, char *buffer, void *callbackData) {
        auto dataPtr = static_cast<WriteCallbackData *>(callbackData);

        auto srcBufBegin = static_cast<std::size_t>(dataPtr->bufferOffset);
        auto srcBufEnd = srcBufBegin + dataPtr->bufferSize;
        auto dstBufBegin = static_cast<std::size_t>(dataPtr->offset);
        auto dstBufEnd = dstBufBegin + static_cast<std::size_t>(bufferSize);
        auto commonBegin = std::max(srcBufBegin, dstBufBegin);
        auto commonEnd = std::min(srcBufEnd, dstBufEnd);

        std::memset(buffer, 0, bufferSize);

        if (dstBufBegin < dataPtr->fileSize) {
            auto size = std::min(dstBufEnd, dataPtr->fileSize);
            dataPtr->helper.sh_read(dataPtr->helperCTX, dataPtr->fileId,
                asio::mutable_buffer(buffer, size), dataPtr->offset);
        }

        if (commonBegin < commonEnd) {
            auto srcBufShift = commonBegin - srcBufBegin;
            auto dstBufShift = commonBegin - dstBufBegin;

            asio::buffer_copy(
                asio::mutable_buffer(buffer, bufferSize) + dstBufShift,
                dataPtr->buffer + srcBufShift, commonEnd - commonBegin);
        }

        return bufferSize;
    };

    S3_put_object(&ctx.bucketCTX, tmpFileId.c_str(), newFileSize, nullptr,
        nullptr, &putObjectHandler, static_cast<void *>(&data));

    sh_copy(ctx, tmpFileId, fileId);
    sh_unlink(ctx, tmpFileId);

    return data.bufferSize;
}

void S3Helper::sh_truncate(
    const S3HelperCTX &ctx, const std::string &fileId, off_t size)
{
    sh_write(ctx, fileId, asio::const_buffer{}, 0, size);
}

std::size_t S3Helper::sh_getFileSize(
    const S3HelperCTX &ctx, const std::string &fileId)
{
    GetFileSizeCallbackData data;
    ResponseHandler<GetFileSizeCallbackData> responseHandler;
    responseHandler.propertiesCallback = [](
        const S3ResponseProperties *properties, void *callbackData) {
        auto dataPtr = static_cast<GetFileSizeCallbackData *>(callbackData);
        dataPtr->fileSize = properties->contentLength;
        return S3StatusOK;
    };

    S3_head_object(&ctx.bucketCTX, fileId.c_str(), nullptr, &responseHandler,
        static_cast<void *>(&data));

    return data.fileSize;
}

void S3Helper::sh_copy(const S3HelperCTX &ctx, const std::string &srcFileId,
    const std::string &dstFileId)
{
    Operation data{"sh_copy"};
    ResponseHandler<Operation> responseHandler;
    S3_copy_object(&ctx.bucketCTX, srcFileId.c_str(), nullptr,
        dstFileId.c_str(), nullptr, nullptr, 0, nullptr, nullptr,
        &responseHandler, static_cast<void *>(&data));
}

std::shared_ptr<S3HelperCTX> S3Helper::getCTX(CTXPtr rawCTX) const
{
    auto ctx = std::dynamic_pointer_cast<S3HelperCTX>(rawCTX);
    if (ctx == nullptr) {
        LOG(INFO) << "Helper changed. Creating new context with arguments: "
                  << m_args;
        return std::make_shared<S3HelperCTX>(rawCTX->parameters(), m_args);
    }
    return ctx;
}

std::string S3Helper::temporaryFileId(const std::string &fileId)
{
    std::stringstream ss;
    ss << fileId << "." << std::time(nullptr);
    return ss.str();
}

error_t S3Helper::makePosixError(std::errc code)
{
    return error_t(static_cast<int>(code), std::system_category());
}

void S3Helper::throwPosixError(const std::string &operation, S3Status status,
    const S3ErrorDetails *errorDetails)
{
    std::stringstream ss;
    ss << "Operation '" << operation << "' completed returning an error: '"
       << S3_get_status_name(status) << "'";
    if (errorDetails != nullptr) {
        if (errorDetails->resource)
            ss << ", resource: '" << errorDetails->resource << "'";
        if (errorDetails->message)
            ss << ", message: '" << errorDetails->message << "'";
        if (errorDetails->furtherDetails)
            ss << ", details: '" << errorDetails->furtherDetails << "'";
        if (errorDetails->extraDetailsCount) {
            ss << ", extras: {";
            for (int i = 0; i < errorDetails->extraDetailsCount; ++i) {
                const auto &detail = errorDetails->extraDetails[i];
                ss << "{'" << detail.name << "', '" << detail.value << "'}";
            }
            ss << "}";
        }
    }
    LOG(ERROR) << ss.str();

    auto result = s_errorsTranslation.find(status);
    if (result != s_errorsTranslation.end())
        throw std::system_error{result->second};
    throw std::system_error{makePosixError(std::errc::io_error)};
}

S3HelperCTX::S3HelperCTX(std::unordered_map<std::string, std::string> params,
    std::unordered_map<std::string, std::string> args)
    : IStorageHelperCTX{std::move(params)}
    , m_args{std::move(args)}
{
    bucketCTX.hostName = m_args.at(S3_HELPER_HOST_NAME_ARG).c_str();
    bucketCTX.bucketName = m_args.at(S3_HELPER_BUCKET_NAME_ARG).c_str();
    bucketCTX.protocol = S3ProtocolHTTP;
    bucketCTX.uriStyle = S3UriStylePath;

    auto result = m_args.find(S3_HELPER_ACCESS_KEY_ARG);
    if (result != m_args.end())
        bucketCTX.accessKeyId = result->second.c_str();

    result = m_args.find(S3_HELPER_SECRET_KEY_ARG);
    if (result != m_args.end())
        bucketCTX.secretAccessKey = result->second.c_str();
}

void S3HelperCTX::setUserCTX(std::unordered_map<std::string, std::string> args)
{
    m_args.swap(args);
    m_args.insert(args.begin(), args.end());
    bucketCTX.hostName = m_args.at(S3_HELPER_HOST_NAME_ARG).c_str();
    bucketCTX.bucketName = m_args.at(S3_HELPER_BUCKET_NAME_ARG).c_str();
    bucketCTX.accessKeyId = m_args.at(S3_HELPER_ACCESS_KEY_ARG).c_str();
    bucketCTX.secretAccessKey = m_args.at(S3_HELPER_SECRET_KEY_ARG).c_str();
}

std::unordered_map<std::string, std::string> S3HelperCTX::getUserCTX()
{
    return {{S3_HELPER_ACCESS_KEY_ARG, m_args.at(S3_HELPER_ACCESS_KEY_ARG)},
        {S3_HELPER_SECRET_KEY_ARG, m_args.at(S3_HELPER_SECRET_KEY_ARG)}};
}

} // namespace helpers
} // namespace one
