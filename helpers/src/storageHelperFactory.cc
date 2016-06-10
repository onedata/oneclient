/**
 * @file storageHelperFactory.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/storageHelperFactory.h"

#include "buffering/bufferAgent.h"
#include "cephHelper.h"
#include "directIOHelper.h"
#include "keyValueAdapter.h"
#include "proxyIOHelper.h"
#include "s3Helper.h"
#include "scheduler.h"
#include "swiftHelper.h"

#ifdef BUILD_PROXY_IO
#include "proxyIOHelper.h"
#endif

namespace one {
namespace helpers {

#ifdef BUILD_PROXY_IO
StorageHelperFactory::StorageHelperFactory(asio::io_service &cephService,
    asio::io_service &dioService, asio::io_service &kvS3Service,
    asio::io_service &kvSwiftService,
    communication::Communicator &communicator,
    std::size_t bufferSchedulerWorkers)
    : m_cephService{cephService}
    , m_dioService{dioService}
    , m_kvS3Service{kvS3Service}
    , m_kvSwiftService{kvSwiftService}
    , m_scheduler{std::make_unique<Scheduler>(bufferSchedulerWorkers)}
    , m_communicator{communicator}
{
}
#else
StorageHelperFactory::StorageHelperFactory(asio::io_service &cephService,
    asio::io_service &dioService, asio::io_service &kvS3Service,
    asio::io_service &kvSwiftService,
    std::size_t bufferSchedulerWorkers)
    : m_cephService{cephService}
    , m_dioService{dioService}
    , m_kvS3Service{kvS3Service}
    , m_kvSwiftService{kvSwiftService}
    , m_scheduler{std::make_unique<Scheduler>(bufferSchedulerWorkers)}
{
}
#endif

StorageHelperFactory::~StorageHelperFactory() = default;

std::shared_ptr<IStorageHelper> StorageHelperFactory::getStorageHelper(
    const std::string &sh_name,
    const std::unordered_map<std::string, std::string> &args)
{
    if (sh_name == CEPH_HELPER_NAME)
        return std::make_shared<buffering::BufferAgent>(
            buffering::BufferLimits{},
            std::make_unique<CephHelper>(args, m_cephService), *m_scheduler);

    if (sh_name == DIRECT_IO_HELPER_NAME) {
#ifdef __linux__
        auto userCTXFactory = DirectIOHelper::linuxUserCTXFactory;
#else
        auto userCTXFactory = DirectIOHelper::noopUserCTXFactory;
#endif
        return std::make_shared<DirectIOHelper>(
            args, m_dioService, userCTXFactory);
    }

#ifdef BUILD_PROXY_IO
    if (sh_name == PROXY_IO_HELPER_NAME)
        return std::make_shared<buffering::BufferAgent>(
            buffering::BufferLimits{},
            std::make_unique<ProxyIOHelper>(args, m_communicator),
            *m_scheduler);
#endif

    if (sh_name == S3_HELPER_NAME)
        return std::make_shared<buffering::BufferAgent>(
            buffering::BufferLimits{},
            std::make_unique<KeyValueAdapter>(
                std::make_unique<S3Helper>(args), m_kvS3Service, m_kvS3Locks),
            *m_scheduler);

    if (sh_name == SWIFT_HELPER_NAME)
        return std::make_shared<buffering::BufferAgent>(
            buffering::BufferLimits{},
            std::make_unique<KeyValueAdapter>(
                std::make_unique<SwiftHelper>(args), m_kvSwiftService, m_kvSwiftLocks),
            *m_scheduler);

    throw std::system_error{std::make_error_code(std::errc::invalid_argument),
        "Invalid storage helper name: '" + sh_name + "'"};
}

} // namespace helpers
} // namespace one
