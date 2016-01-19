/**
 * @file s3Helper.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_S3_HELPER_H
#define HELPERS_S3_HELPER_H

#include "helpers/IStorageHelper.h"

#include "libs3.h"

#include <asio.hpp>

#include <map>
#include <mutex>

namespace one {
namespace helpers {

class S3HelperCTX : public IStorageHelperCTX {
public:
    S3HelperCTX(std::unordered_map<std::string, std::string> args);

    void setUserCTX(std::unordered_map<std::string, std::string> args);

    std::unordered_map<std::string, std::string> getUserCTX();

    int getFlagValue(Flag flag) { return 0; }

    void setFlags(int flags) {}

    S3BucketContext bucketCTX;

private:
    std::unordered_map<std::string, std::string> m_args;
};

class S3Helper : public IStorageHelper {
public:
    S3Helper(std::unordered_map<std::string, std::string> args,
        asio::io_service &service);

    ~S3Helper();

    CTXPtr createCTX();

    void ash_open(CTXPtr ctx, const boost::filesystem::path &p,
        GeneralCallback<int> callback)
    {
        callback(-1, SUCCESS_CODE);
    }

    void ash_unlink(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback);

    void ash_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset,
        GeneralCallback<asio::mutable_buffer>);

    void ash_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, GeneralCallback<std::size_t>);

    void ash_truncate(CTXPtr ctx, const boost::filesystem::path &p, off_t size,
        VoidCallback callback);

    void ash_mknod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        dev_t rdev, VoidCallback callback)
    {
        callback(SUCCESS_CODE);
    }

    void ash_mkdir(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback)
    {
        callback(SUCCESS_CODE);
    }

    void ash_chmod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        VoidCallback callback)
    {
        callback(SUCCESS_CODE);
    }

    void sh_unlink(const S3HelperCTX &ctx, const std::string &fileId);

    asio::mutable_buffer sh_read(const S3HelperCTX &ctx,
        const std::string &fileId, asio::mutable_buffer buf, off_t offset);

    std::size_t sh_write(const S3HelperCTX &ctx, const std::string &fileId,
        asio::const_buffer buf, off_t offset);

    std::size_t sh_write(const S3HelperCTX &ctx, const std::string &fileId,
                         asio::const_buffer buf, off_t offset, std::size_t fileSize);

    void sh_truncate(const S3HelperCTX &ctx, const std::string &fileId, off_t size);

    std::size_t sh_getFileSize(
        const S3HelperCTX &ctx, const std::string &fileId);

    void sh_copy(const S3HelperCTX &ctx, const std::string &srcFileId,
        const std::string &dstFileId);

private:
    std::shared_ptr<S3HelperCTX> getCTX(CTXPtr rawCTX) const;

    std::string temporaryFileId(const std::string &fileId);

    static error_t makePosixError(std::errc code);

    static void throwPosixError(const std::string &operation, S3Status status,
        const S3ErrorDetails *error);

    asio::io_service &m_service;
    std::unordered_map<std::string, std::string> m_args;
    static std::mutex s_mutex;
    static const std::map<int, error_t> s_errorsTranslation;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_S3_HELPER_H
