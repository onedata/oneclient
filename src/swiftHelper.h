/**
 * @file swiftHelper.h
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_SWIFT_HELPER_H
#define HELPERS_SWIFT_HELPER_H

#include "asioExecutor.h"
#include "keyValueAdapter.h"
#include "keyValueHelper.h"

#include "Swift/Account.h"
#include "Swift/Container.h"
#include "Swift/HTTPIO.h"
#include "Swift/Object.h"

#include <mutex>
#include <vector>

namespace one {
namespace helpers {

class SwiftHelper;

/**
 * An implementation of @c StorageHelperFactory for Swift storage helper.
 */
class SwiftHelperFactory : public StorageHelperFactory {
public:
    /**
     * Constructor.
     * @param service @c io_service that will be used for some async operations.
     */
    SwiftHelperFactory(asio::io_service &service)
        : m_service{service}
    {
    }

    std::shared_ptr<StorageHelper> createStorageHelper(
        const Params &parameters) override
    {
        const auto &authUrl = getParam(parameters, "authUrl");
        const auto &containerName = getParam(parameters, "containerName");
        const auto &tenantName = getParam(parameters, "tenantName");
        const auto &userName = getParam(parameters, "username");
        const auto &password = getParam(parameters, "password");
        Timeout timeout{getParam<std::size_t>(
            parameters, "timeout", ASYNC_OPS_TIMEOUT.count())};
        const auto &blockSize =
            getParam<std::size_t>(parameters, "blockSize", DEFAULT_BLOCK_SIZE);

        return std::make_shared<KeyValueAdapter>(
            std::make_shared<SwiftHelper>(containerName, authUrl, tenantName,
                userName, password, std::move(timeout)),
            std::make_shared<AsioExecutor>(m_service), blockSize);
    }

private:
    asio::io_service &m_service;
};

/**
* The SwiftHelper class provides access to Swift storage system.
*/
class SwiftHelper : public KeyValueHelper {
public:
    /**
     * Constructor.
     * @param authUrl The URL for authorization with Swift.
     * @param containerName Name of the used container.
     * @param tenantName Name of the tenant.
     * @param userName Name of the Swift user.
     * @param password Password of the Swift user.
     */
    SwiftHelper(folly::fbstring containerName, const folly::fbstring &authUrl,
        const folly::fbstring &tenantName, const folly::fbstring &userName,
        const folly::fbstring &password, Timeout timeout = ASYNC_OPS_TIMEOUT);

    folly::IOBufQueue getObject(const folly::fbstring &key, const off_t offset,
        const std::size_t size) override;

    off_t getObjectsSize(
        const folly::fbstring &prefix, const std::size_t objectSize) override;

    std::size_t putObject(
        const folly::fbstring &key, folly::IOBufQueue buf) override;

    void deleteObjects(const folly::fbvector<folly::fbstring> &keys) override;

    folly::fbvector<folly::fbstring> listObjects(
        const folly::fbstring &prefix) override;

    const Timeout &timeout() override { return m_timeout; }

private:
    class Authentication {
    public:
        Authentication(const folly::fbstring &authUrl,
            const folly::fbstring &tenantName, const folly::fbstring &userName,
            const folly::fbstring &password);

        Swift::Account &getAccount();

    private:
        std::mutex m_authMutex;
        Swift::AuthenticationInfo m_authInfo;
        std::shared_ptr<Swift::Account> m_account;
    } m_auth;

    folly::fbstring m_containerName;
    Timeout m_timeout;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_SWIFT_HELPER_H
