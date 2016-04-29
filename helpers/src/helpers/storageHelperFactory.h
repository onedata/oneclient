/**
 * @file storageHelperFactory.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_STORAGE_HELPER_FACTORY_H
#define HELPERS_STORAGE_HELPER_FACTORY_H

#include "helpers/IStorageHelper.h"

#include <asio/io_service.hpp>
#include <boost/optional.hpp>

#include <memory>
#include <string>

namespace one {
namespace helpers {

namespace proxyio {
class BufferAgent;
} // namespace proxyio

constexpr auto CEPH_HELPER_NAME = "Ceph";
constexpr auto DIRECT_IO_HELPER_NAME = "DirectIO";
constexpr auto PROXY_IO_HELPER_NAME = "ProxyIO";
constexpr auto S3_HELPER_NAME = "AmazonS3";

/**
 * Factory providing objects of requested storage helpers.
 */
class StorageHelperFactory {
public:
    StorageHelperFactory(asio::io_service &ceph_service,
        asio::io_service &dio_service, asio::io_service &s3Service,
        std::shared_ptr<proxyio::BufferAgent> bufferAgent);

    virtual ~StorageHelperFactory() = default;

    /**
     * Produces storage helper object.
     * @param sh Name of storage helper that has to be returned.
     * @param args Arguments map passed as argument to storge helper's
     * constructor.
     * @return Pointer to storage helper object along with its ownership.
     */
    virtual std::shared_ptr<IStorageHelper> getStorageHelper(
        const std::string &sh,
        const std::unordered_map<std::string, std::string> &args);

private:
    asio::io_service &m_cephService;
    asio::io_service &m_dioService;
    asio::io_service &m_s3Service;
    std::shared_ptr<proxyio::BufferAgent> m_bufferAgent;
};

} // namespace helpers
} // namespace one

#endif // HELPERS_STORAGE_HELPER_FACTORY_H
