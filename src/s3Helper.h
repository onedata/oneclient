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
    S3HelperCTX(std::unordered_map<std::string, std::string> params,
        std::unordered_map<std::string, std::string> args);

    /**
     * @copydoc IStorageHelper::setUserCtx
     * Args should contain 'access_key' and 'secret_key' values.
     */
    void setUserCTX(std::unordered_map<std::string, std::string> args) override;

    std::unordered_map<std::string, std::string> getUserCTX() override;

private:
    std::unordered_map<std::string, std::string> m_args;
};

class S3Helper : public KeyValueHelper {
public:
    S3Helper(std::unordered_map<std::string, std::string> args);

    CTXPtr createCTX(
        std::unordered_map<std::string, std::string> params) override;

    asio::mutable_buffer getObject(CTXPtr ctx, std::string key,
        asio::mutable_buffer buf, off_t offset) override;

    std::size_t putObject(
        CTXPtr ctx, std::string key, asio::const_buffer buf) override;

    void deleteObjects(CTXPtr ctx, std::vector<std::string> keys) override;

    std::vector<std::string> listObjects(
        CTXPtr ctx, std::string prefix) override;

private:
    std::shared_ptr<S3HelperCTX> getCTX(CTXPtr rawCTX) const;

    std::unordered_map<std::string, std::string> m_args;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_S3_HELPER_H
