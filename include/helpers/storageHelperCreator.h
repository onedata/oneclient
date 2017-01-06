/**
 * @file storageHelperCreator.h
 * @author Rafal Slota
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_STORAGE_HELPER_FACTORY_H
#define HELPERS_STORAGE_HELPER_FACTORY_H

#include "storageHelper.h"

#ifdef BUILD_PROXY_IO
#include "communication/communicator.h"
#endif

#include <asio/io_service.hpp>
#include <boost/optional.hpp>
#include <tbb/concurrent_hash_map.h>

#include <memory>
#include <string>

namespace one {

class Scheduler;

namespace helpers {

constexpr auto CEPH_HELPER_NAME = "ceph";
constexpr auto POSIX_HELPER_NAME = "posix";
constexpr auto PROXY_HELPER_NAME = "proxy";
constexpr auto S3_HELPER_NAME = "s3";
constexpr auto SWIFT_HELPER_NAME = "swift";

/**
 * Factory providing objects of requested storage helpers.
 */
class StorageHelperCreator {
public:
#ifdef BUILD_PROXY_IO
    StorageHelperCreator(asio::io_service &ceph_service,
        asio::io_service &dio_service, asio::io_service &kvS3Service,
        asio::io_service &kvSwiftService,
        communication::Communicator &m_communicator,
        std::size_t bufferSchedulerWorkers = 1);
#else
    StorageHelperCreator(asio::io_service &ceph_service,
        asio::io_service &dio_service, asio::io_service &kvS3Service,
        asio::io_service &kvSwiftService,
        std::size_t bufferSchedulerWorkers = 1);
#endif

    virtual ~StorageHelperCreator();

    /**
     * Produces storage helper object.
     * @param sh Name of storage helper that has to be returned.
     * @param args Arguments map passed as argument to storge helper's
     * constructor.
     * @return The created storage helper object.
     */
    virtual std::shared_ptr<StorageHelper> getStorageHelper(
        const folly::fbstring &sh,
        const std::unordered_map<folly::fbstring, folly::fbstring> &args,
        const bool buffered = false);

private:
    asio::io_service &m_cephService;
    asio::io_service &m_dioService;
    asio::io_service &m_s3Service;
    asio::io_service &m_swiftService;
    std::unique_ptr<Scheduler> m_scheduler;

#ifdef BUILD_PROXY_IO
    communication::Communicator &m_communicator;
#endif
};

} // namespace helpers
} // namespace one

#endif // HELPERS_STORAGE_HELPER_FACTORY_H
