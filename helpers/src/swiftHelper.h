/**
 * @file swiftHelper.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_SWIFT_HELPER_H
#define HELPERS_SWIFT_HELPER_H

#include "keyValueHelper.h"

#include "Swift/Account.h"
#include "Swift/Container.h"
#include "Swift/HTTPIO.h"
#include "Swift/Object.h"

#include <mutex>
#include <vector>

namespace one {
namespace helpers {

constexpr auto SWIFT_HELPER_AUTH_URL_ARG = "auth_url";
constexpr auto SWIFT_HELPER_CONTAINER_NAME_ARG = "container_name";
constexpr auto SWIFT_HELPER_TENANT_NAME_ARG = "tenant_name";
constexpr auto SWIFT_HELPER_USER_NAME_ARG = "user_name";
constexpr auto SWIFT_HELPER_PASSWORD_ARG = "password";

/**
* The SwiftHelperCTX class represents context for Swift helpers and its object
* is passed to all helper functions.
*/
class SwiftHelperCTX : public IStorageHelperCTX {
public:
    /**
     * Constructor.
     * @param args Map with parameters required to create context. It should
     * contain at least 'auth_url', 'container_name' and 'tenant_name' values.
     * Additionally default 'user_name' and 'password' can be passed,
     * which will be used if user context has not been set.
     */
    SwiftHelperCTX(std::unordered_map<std::string, std::string> params,
        std::unordered_map<std::string, std::string> args);

    /**
     * @copydoc IStorageHelper::setUserCtx
     * Args should contain 'username' and 'password' values.
     */
    void setUserCTX(std::unordered_map<std::string, std::string> args) override;

    std::unordered_map<std::string, std::string> getUserCTX() override;

    /*
     * Authenticates user within swift storage.
     */
    const std::unique_ptr<Swift::Account> &authenticate();

    /*
     * Returns container name.
     * @return
     */
    const std::string &getContainerName() const;

private:
    std::unique_ptr<Swift::Account> m_account;
    std::unordered_map<std::string, std::string> m_args;
    std::mutex m_mutex;
};

class SwiftHelper : public KeyValueHelper {
public:
    SwiftHelper(std::unordered_map<std::string, std::string> args);

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
    std::shared_ptr<SwiftHelperCTX> getCTX(CTXPtr rawCTX) const;

    std::unordered_map<std::string, std::string> m_args;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_SWIFT_HELPER_H
