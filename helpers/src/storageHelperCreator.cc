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
#include "posixHelper.h"
#include "proxyHelper.h"
#include "s3Helper.h"
#include "scheduler.h"
#include "swiftHelper.h"

namespace one {
namespace helpers {

#ifdef BUILD_PROXY_IO
StorageHelperCreator::StorageHelperCreator(asio::io_service &cephService,
    asio::io_service &dioService, asio::io_service &s3Service,
    asio::io_service &swiftService, communication::Communicator &communicator,
    std::size_t bufferSchedulerWorkers, buffering::BufferLimits bufferLimits)
    : m_cephService{cephService}
    , m_dioService{dioService}
    , m_s3Service{s3Service}
    , m_swiftService{swiftService}
    , m_scheduler{std::make_unique<Scheduler>(bufferSchedulerWorkers)}
    , m_bufferLimits{std::move(bufferLimits)}
    , m_communicator{communicator}
{
}
#else
StorageHelperCreator::StorageHelperCreator(asio::io_service &cephService,
    asio::io_service &dioService, asio::io_service &s3Service,
    asio::io_service &swiftService, std::size_t bufferSchedulerWorkers,
    buffering::BufferLimits bufferLimits)
    : m_cephService{cephService}
    , m_dioService{dioService}
    , m_s3Service{s3Service}
    , m_swiftService{swiftService}
    , m_scheduler{std::make_unique<Scheduler>(bufferSchedulerWorkers)}
    , m_bufferLimits{std::move(bufferLimits)}
{
}
#endif

StorageHelperCreator::~StorageHelperCreator() = default;

std::shared_ptr<StorageHelper> StorageHelperCreator::getStorageHelper(
    const folly::fbstring &name,
    const std::unordered_map<folly::fbstring, folly::fbstring> &args,
    const bool buffered)
{
    StorageHelperPtr helper;

    if (name == POSIX_HELPER_NAME)
        helper = PosixHelperFactory{m_dioService}.createStorageHelper(args);

    if (name == CEPH_HELPER_NAME)
        helper = CephHelperFactory{m_cephService}.createStorageHelper(args);

#ifdef BUILD_PROXY_IO
    if (name == PROXY_HELPER_NAME)
        helper = ProxyHelperFactory{m_communicator}.createStorageHelper(args);
#endif

    if (name == S3_HELPER_NAME)
        helper = S3HelperFactory{m_s3Service}.createStorageHelper(args);

    if (name == SWIFT_HELPER_NAME)
        helper = SwiftHelperFactory{m_swiftService}.createStorageHelper(args);

    if (!helper)
        throw std::system_error{
            std::make_error_code(std::errc::invalid_argument),
            "Invalid storage helper name: '" + name.toStdString() + "'"};

    if (buffered)
        return std::make_shared<buffering::BufferAgent>(
            m_bufferLimits, helper, *m_scheduler);

    return helper;
}

} // namespace helpers
} // namespace one
