/**
 * @file storageHelperCreator.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/storageHelperCreator.h"

#include "buffering/bufferAgent.h"
#include "cephHelper.h"
#include "directIOHelper.h"
#include "proxyIOHelper.h"
#include "s3Helper.h"
#include "scheduler.h"
#include "swiftHelper.h"

namespace one {
namespace helpers {

#ifdef BUILD_PROXY_IO
StorageHelperCreator::StorageHelperCreator(asio::io_service &cephService,
    asio::io_service &dioService, asio::io_service &s3Service,
    asio::io_service &swiftService, communication::Communicator &communicator,
    std::size_t bufferSchedulerWorkers)
    : m_cephService{cephService}
    , m_dioService{dioService}
    , m_s3Service{s3Service}
    , m_swiftService{swiftService}
    , m_scheduler{std::make_unique<Scheduler>(bufferSchedulerWorkers)}
    , m_communicator{communicator}
{
}
#else
StorageHelperCreator::StorageHelperCreator(asio::io_service &cephService,
    asio::io_service &dioService, asio::io_service &s3Service,
    asio::io_service &swiftService, std::size_t bufferSchedulerWorkers)
    : m_cephService{cephService}
    , m_dioService{dioService}
    , m_s3Service{s3Service}
    , m_swiftService{swiftService}
    , m_scheduler{std::make_unique<Scheduler>(bufferSchedulerWorkers)}
{
}
#endif

StorageHelperCreator::~StorageHelperCreator() = default;

std::shared_ptr<StorageHelper> StorageHelperCreator::getStorageHelper(
    const folly::fbstring &sh_name,
    const std::unordered_map<folly::fbstring, folly::fbstring> &args,
    const bool buffered)
{
    if (sh_name == DIRECT_IO_HELPER_NAME)
        return DirectIOHelperFactory{m_dioService}.createStorageHelper(args);

    StorageHelperPtr helper;

    if (sh_name == CEPH_HELPER_NAME)
        helper = CephHelperFactory{m_cephService}.createStorageHelper(args);

#ifdef BUILD_PROXY_IO
    if (sh_name == PROXY_IO_HELPER_NAME)
        helper = ProxyIOHelperFactory{m_communicator}.createStorageHelper(args);
#endif

    if (sh_name == S3_HELPER_NAME)
        helper = S3HelperFactory{m_s3Service}.createStorageHelper(args);

    if (sh_name == SWIFT_HELPER_NAME)
        helper = SwiftHelperFactory{m_swiftService}.createStorageHelper(args);

    if (!helper)
        throw std::system_error{
            std::make_error_code(std::errc::invalid_argument),
            "Invalid storage helper name: '" + sh_name.toStdString() + "'"};

    if (buffered)
        return std::make_shared<buffering::BufferAgent>(
            buffering::BufferLimits{}, helper, *m_scheduler);

    return helper;
}

} // namespace helpers
} // namespace one
