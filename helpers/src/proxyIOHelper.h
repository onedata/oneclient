/**
 * @file proxyIOHelper.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_PROXY_IO_HELPER_H
#define HELPERS_PROXY_IO_HELPER_H

#include "helpers/storageHelper.h"

#include "communication/communicator.h"

#include <asio/io_service.hpp>

#include <cstdint>

namespace one {
namespace helpers {

/**
 * The @c FileHandle implementation for ProxyIO storage helper.
 */
class ProxyIOFileHandle : public FileHandle {
public:
    /**
     * Constructor.
     * @param fileId Helper-specific ID of the open file.
     * @param storageId Id of the storage the file is stored on.
     * @param openParams Parameters associated with the handle.
     * @param communicator Communicator that will be used for communication
     * with a provider.
     */
    ProxyIOFileHandle(folly::fbstring fileId, folly::fbstring storageId,
        Params openParams, communication::Communicator &communicator,
        Timeout timeout);

    folly::Future<folly::IOBufQueue> read(
        const off_t offset, const std::size_t size) override;

    folly::Future<std::size_t> write(
        const off_t offset, folly::IOBufQueue buf) override;

    folly::Future<std::size_t> multiwrite(
        folly::fbvector<std::pair<off_t, folly::IOBufQueue>> buffs) override;

    const Timeout &timeout() override { return m_timeout; }

private:
    folly::fbstring m_storageId;
    communication::Communicator &m_communicator;
    Timeout m_timeout;
};

/**
 * @c ProxyIOHelper is responsible for providing a POSIX-like API for operations
 * on files proxied through a onedata provider.
 */
class ProxyIOHelper : public StorageHelper {
public:
    /**
     * Constructor.
     * @param storageId Id of the storage the file is stored on.
     * @param communicator Communicator that will be used for communication
     * with a provider.
     */
    ProxyIOHelper(folly::fbstring storageId,
        communication::Communicator &communicator,
        Timeout timeout = ASYNC_OPS_TIMEOUT);

    folly::Future<FileHandlePtr> open(const folly::fbstring &fileId,
        const int flags, const Params &openParams) override;

    const Timeout &timeout() override { return m_timeout; }

private:
    folly::fbstring m_storageId;
    communication::Communicator &m_communicator;
    Timeout m_timeout;
};

/**
 * An implementation of @c StorageHelperFactory for ProxyIO storage helper.
 */
class ProxyIOHelperFactory : public StorageHelperFactory {
public:
    /**
     * Constructor.
     * @param communicator Communicator that will be used for communication
     * with a provider.
     */
    ProxyIOHelperFactory(communication::Communicator &communicator)
        : m_communicator{communicator}
    {
    }

    std::shared_ptr<StorageHelper> createStorageHelper(
        const Params &parameters) override
    {
        auto storageId = getParam(parameters, "storage_id");
        Timeout timeout{getParam<std::size_t>(
            parameters, "timeout", ASYNC_OPS_TIMEOUT.count())};

        return std::make_shared<ProxyIOHelper>(
            std::move(storageId), m_communicator, std::move(timeout));
    }

private:
    communication::Communicator &m_communicator;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_PROXY_IO_HELPER_H
