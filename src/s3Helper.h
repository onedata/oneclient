/**
 * @file s3Helper.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_S3_HELPER_H
#define HELPERS_S3_HELPER_H

#include "keyValueHelper.h"

#include <aws/core/Aws.h>
#include <aws/s3/S3Errors.h>

#include <map>
#include <sstream>

namespace Aws {
namespace S3 {
class S3Client;
}
}

namespace one {
namespace helpers {

constexpr auto S3_HELPER_SCHEME_ARG = "scheme";
constexpr auto S3_HELPER_HOST_NAME_ARG = "host_name";
constexpr auto S3_HELPER_BUCKET_NAME_ARG = "bucket_name";
constexpr auto S3_HELPER_ACCESS_KEY_ARG = "access_key";
constexpr auto S3_HELPER_SECRET_KEY_ARG = "secret_key";

class S3HelperApiInit {
public:
    S3HelperApiInit() { Aws::InitAPI(m_options); }

    ~S3HelperApiInit() { Aws::ShutdownAPI(m_options); }

private:
    Aws::SDKOptions m_options;
};

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
     * if user context has not been set. It is also possible to overwrite http
     * client 'scheme', default is 'https'.
     */
    S3HelperCTX(std::unordered_map<std::string, std::string> params,
        std::unordered_map<std::string, std::string> args);

    /**
     * @copydoc IStorageHelper::setUserCtx
     * Args should contain 'access_key' and 'secret_key' values.
     */
    void setUserCTX(std::unordered_map<std::string, std::string> args) override;

    std::unordered_map<std::string, std::string> getUserCTX() override;

    const std::string &getBucket() const;

    const std::unique_ptr<Aws::S3::S3Client> &getClient() const;

private:
    void init();

    std::unordered_map<std::string, std::string> m_args;
    std::unique_ptr<Aws::S3::S3Client> m_client;
};

/**
* The S3Helper class provides access to Simple Storage Service (S3) via AWS SDK.
*/
class S3Helper : public KeyValueHelper {
public:
    S3Helper(std::unordered_map<std::string, std::string> args);

    CTXPtr createCTX(
        std::unordered_map<std::string, std::string> params) override;

    asio::mutable_buffer getObject(CTXPtr ctx, std::string key,
        asio::mutable_buffer buf, off_t offset) override;

    off_t getObjectsSize(
        CTXPtr ctx, const std::string &prefix, std::size_t objectSize) override;

    std::size_t putObject(
        CTXPtr ctx, std::string key, asio::const_buffer buf) override;

    void deleteObjects(CTXPtr ctx, std::vector<std::string> keys) override;

    std::vector<std::string> listObjects(
        CTXPtr ctx, std::string prefix) override;

private:
    std::shared_ptr<S3HelperCTX> getCTX(CTXPtr ctx) const;

    template <typename Outcome> error_t getReturnCode(const Outcome &outcome)
    {
        if (outcome.IsSuccess())
            return SUCCESS_CODE;

        auto error = std::errc::io_error;
        auto search = s_errors.find(outcome.GetError().GetErrorType());
        if (search != s_errors.end())
            error = search->second;

        return std::error_code(static_cast<int>(error), std::system_category());
    }

    template <typename Outcome>
    void throwOnError(std::string operation, const Outcome &outcome)
    {
        auto code = getReturnCode(outcome);

        if (code == SUCCESS_CODE)
            return;

        std::stringstream ss;
        ss << "'" << operation << "': " << outcome.GetError().GetMessage();

        throw std::system_error{code, ss.str()};
    }

    std::unordered_map<std::string, std::string> m_args;
    static std::map<Aws::S3::S3Errors, std::errc> s_errors;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_S3_HELPER_H
