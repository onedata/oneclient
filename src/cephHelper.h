/**
 * @file cephHelper.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_CEPH_HELPER_H
#define HELPERS_CEPH_HELPER_H

#include "helpers/storageHelper.h"

#include "asioExecutor.h"
#include "logging.h"

#include <asio.hpp>
#include <folly/Executor.h>
#include <rados/librados.hpp>
#include <radosstriper/libradosstriper.hpp>

namespace one {
namespace helpers {

class CephHelper;

/**
 * The @c FileHandle implementation for Ceph storage helper.
 */
class CephFileHandle : public FileHandle,
                       public std::enable_shared_from_this<CephFileHandle> {
public:
    /**
     * Constructor.
     * @param fileId Ceph-specific ID associated with the file.
     * @param helper A pointer to the helper that created the handle.
     * @param ioCTX A reference to @c librados::IoCtx for async operations.
     */
    CephFileHandle(folly::fbstring fileId, std::shared_ptr<CephHelper> helper,
        librados::IoCtx &ioCTX);

    folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) override;

    folly::Future<std::size_t> write(
        const off_t offset, folly::IOBufQueue buf) override;

    const Timeout &timeout() override;

private:
    std::shared_ptr<CephHelper> m_helper;
    librados::IoCtx &m_ioCTX;
};

/**
 * The CephHelper class provides access to Ceph storage via librados library.
 */
class CephHelper : public StorageHelper,
                   public std::enable_shared_from_this<CephHelper> {
public:
    /**
     * Constructor.
     * @param clusterName Name of the Ceph cluster to connect to.
     * @param monHost Name of the Ceph monitor host.
     * @param poolName Name of the Ceph pool to use.
     * @param userName Name of the Ceph user.
     * @param key Secret key of the Ceph user.
     * @param executor Executor that will drive the helper's async operations.
     */
    CephHelper(folly::fbstring clusterName, folly::fbstring monHost,
        folly::fbstring poolName, folly::fbstring userName, folly::fbstring key,
        std::unique_ptr<folly::Executor> executor,
        Timeout timeout = ASYNC_OPS_TIMEOUT);

    /**
     * Destructor.
     * Closes connection to Ceph storage cluster and destroys internal context
     * object.
     */
    ~CephHelper();

    folly::Future<FileHandlePtr> open(
        const folly::fbstring &fileId, const int, const Params &) override;

    folly::Future<folly::Unit> unlink(const folly::fbstring &fileId) override;

    folly::Future<folly::Unit> truncate(
        const folly::fbstring &fileId, const off_t size) override;

    folly::Future<folly::Unit> mknod(const folly::fbstring &fileId,
        const mode_t mode, const FlagsSet &flags, const dev_t rdev) override
    {
        return folly::makeFuture();
    }

    folly::Future<folly::Unit> mkdir(
        const folly::fbstring &fileId, const mode_t mode) override
    {
        return folly::makeFuture();
    }

    folly::Future<folly::Unit> chmod(
        const folly::fbstring &fileId, const mode_t mode) override
    {
        return folly::makeFuture();
    }

    folly::Future<folly::fbstring> getxattr(
        const folly::fbstring &fileId, const folly::fbstring &name) override;

    folly::Future<folly::Unit> setxattr(const folly::fbstring &fileId,
        const folly::fbstring &name, const folly::fbstring &value, bool create,
        bool replace) override;

    folly::Future<folly::Unit> removexattr(
        const folly::fbstring &fileId, const folly::fbstring &name) override;

    folly::Future<folly::fbvector<folly::fbstring>> listxattr(
        const folly::fbstring &fileId) override;

    const Timeout &timeout() override { return m_timeout; }

    libradosstriper::RadosStriper &getRadosStriper() { return m_radosStriper; }

    /**
     * Establishes connection to the Ceph storage cluster.
     */
    folly::Future<folly::Unit> connect();

private:
    folly::fbstring m_clusterName;
    folly::fbstring m_monHost;
    folly::fbstring m_poolName;
    folly::fbstring m_userName;
    folly::fbstring m_key;

    std::unique_ptr<folly::Executor> m_executor;
    Timeout m_timeout;

    librados::Rados m_cluster;
    librados::IoCtx m_ioCTX;
    libradosstriper::RadosStriper m_radosStriper;
    std::mutex m_connectionMutex;
    bool m_connected = false;
};

/**
 * An implementation of @c StorageHelperFactory for Ceph storage helper.
 */
class CephHelperFactory : public StorageHelperFactory {
public:
    /**
     * Constructor.
     * @param service @c io_service that will be used for some async operations.
     */
    CephHelperFactory(asio::io_service &service)
        : m_service{service}
    {
        LOG_FCALL();
    }

    std::shared_ptr<StorageHelper> createStorageHelper(
        const Params &parameters) override
    {
        const auto &clusterName = getParam(parameters, "clusterName");
        const auto &monHost = getParam(parameters, "monitorHostname");
        const auto &poolName = getParam(parameters, "poolName");
        const auto &userName = getParam(parameters, "username");
        const auto &key = getParam(parameters, "key");
        Timeout timeout{getParam<std::size_t>(
            parameters, "timeout", ASYNC_OPS_TIMEOUT.count())};

        LOG_FCALL() << LOG_FARG(clusterName) << LOG_FARG(monHost)
                    << LOG_FARG(poolName) << LOG_FARG(userName)
                    << LOG_FARG(key);

        return std::make_shared<CephHelper>(clusterName, monHost, poolName,
            userName, key, std::make_unique<AsioExecutor>(m_service),
            std::move(timeout));
    }

private:
    asio::io_service &m_service;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_CEPH_HELPER_H
