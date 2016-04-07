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

#include <asio.hpp>
#include <libs3.h>

#include <map>
#include <mutex>

namespace one {
namespace helpers {

constexpr auto S3_HELPER_HOST_NAME_ARG = "host_name";
constexpr auto S3_HELPER_BUCKET_NAME_ARG = "bucket_name";
constexpr auto S3_HELPER_ACCESS_KEY_ARG = "access_key";
constexpr auto S3_HELPER_SECRET_KEY_ARG = "secret_key";

/**
* The S3HelperCTX class represents context for S3 helpers and its object is
* passed to all helper functions.
*/
class S3HelperCTX : public IStorageHelperCTX {
public:
    /**
     * Constructor.
     * @param args Map with parameters required to create context. It should
     * contain at least 'host_name' and 'bucket_name' values. Additionally
     * default 'access_key' and 'secret_key' can be passed, which will be used
     * if user context has not been set.
     */
    S3HelperCTX(std::unordered_map<std::string, std::string> args);

    /**
     * @copydoc IStorageHelper::setUserCtx
     * Args should contain 'access_key' and 'secret_key' values.
     */
    void setUserCTX(std::unordered_map<std::string, std::string> args) override;

    std::unordered_map<std::string, std::string> getUserCTX() override;

    S3BucketContext bucketCTX = {};

private:
    std::unordered_map<std::string, std::string> m_args;
};

/**
* The S3Helper class provides access to Simple Storage Service (S3) via libs3
* library.
*/
class S3Helper : public IStorageHelper {
public:
    /**
     * Constructor.
     * Initializes S3 library. This is done on a static shared mutex, because
     * initialization function is not thread-safe.
     * @param args Map with parameters required to create helper.
     * @param service Reference to IO service used by the helper.
     */
    S3Helper(std::unordered_map<std::string, std::string> args,
        asio::io_service &service);

    /**
     * Destructor.
     * Deinitializes S3 library.
     */
    ~S3Helper();

    CTXPtr createCTX();

    void ash_open(CTXPtr ctx, const boost::filesystem::path &p, int flags,
        GeneralCallback<int> callback)
    {
        callback(0, SUCCESS_CODE);
    }

    void ash_unlink(
        CTXPtr ctx, const boost::filesystem::path &p, VoidCallback callback);

    void ash_read(CTXPtr ctx, const boost::filesystem::path &p,
        asio::mutable_buffer buf, off_t offset, const std::string &fileUuid,
        GeneralCallback<asio::mutable_buffer>);

    void ash_write(CTXPtr ctx, const boost::filesystem::path &p,
        asio::const_buffer buf, off_t offset, const std::string &fileUuid,
        GeneralCallback<std::size_t>);

    void ash_truncate(CTXPtr ctx, const boost::filesystem::path &p, off_t size,
        VoidCallback callback);

    void ash_mknod(CTXPtr ctx, const boost::filesystem::path &p, mode_t mode,
        FlagsSet flags, dev_t rdev, VoidCallback callback)
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

    void sh_truncate(
        const S3HelperCTX &ctx, const std::string &fileId, off_t size);

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

    struct Operation {
        Operation(std::string _operation)
            : operation{std::move(_operation)}
        {
        }

        std::string operation;
    };

    template <typename T> struct ResponseHandler : S3ResponseHandler {
        ResponseHandler()
            : S3ResponseHandler{[](const S3ResponseProperties *properties,
                                    void *callbackData) { return S3StatusOK; },
                  [](S3Status status, const S3ErrorDetails *errorDetails,
                      void *callbackData) {
                      if (status != S3StatusOK) {
                          auto dataPtr = static_cast<T *>(callbackData);
                          throwPosixError(
                              dataPtr->operation, status, errorDetails);
                      }
                  }}
        {
        }
    };

    struct GetFileSizeCallbackData {
        std::string operation{"sh_getFileSize"};
        std::size_t fileSize;
    };

    struct ReadCallbackData {
        std::string operation{"sh_read"};
        std::size_t size = 0;
        asio::mutable_buffer buffer;
    };

    struct WriteCallbackData {
        WriteCallbackData(const std::string &_fileId,
            const S3HelperCTX &_helperCTX, S3Helper &_helper)
            : fileId{_fileId}
            , helperCTX{_helperCTX}
            , helper{_helper}
        {
        }

        std::string operation{"sh_write"};
        off_t offset = 0;
        std::size_t fileSize;
        off_t bufferOffset;
        std::size_t bufferSize;
        asio::const_buffer buffer;
        const std::string &fileId;
        const S3HelperCTX &helperCTX;
        S3Helper &helper;
    };

    asio::io_service &m_service;
    std::unordered_map<std::string, std::string> m_args;
    static std::mutex s_mutex;
    static const std::map<int, error_t> s_errorsTranslation;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_S3_HELPER_H
