/**
 * @file storageHelperCreator.cc
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpers/storageHelperCreator.h"

#include "buffering/bufferAgent.h"
#include "posixHelper.h"
#include "proxyHelper.h"
#include "scheduler.h"

#if WITH_CEPH
#include "cephHelper.h"
#endif

#if WITH_S3
#include "s3Helper.h"
#endif

#if WITH_SWIFT
#include "swiftHelper.h"
#endif

#if WITH_GLUSTERFS
#include "glusterfsHelper.h"
#endif

namespace one {
namespace helpers {

#ifdef BUILD_PROXY_IO

StorageHelperCreator::StorageHelperCreator(
#if WITH_CEPH
    asio::io_service &cephService,
#endif
    asio::io_service &dioService,
#if WITH_S3
    asio::io_service &s3Service,
#endif
#if WITH_SWIFT
    asio::io_service &swiftService,
#endif
#if WITH_GLUSTERFS
    asio::io_service &glusterfsService,
#endif
    communication::Communicator &communicator,
    std::size_t bufferSchedulerWorkers, buffering::BufferLimits bufferLimits)
    :
#if WITH_CEPH
    m_cephService{cephService}
    ,
#endif
    m_dioService{dioService}
    ,
#if WITH_S3
    m_s3Service{s3Service}
    ,
#endif
#if WITH_SWIFT
    m_swiftService{swiftService}
    ,
#endif
#if WITH_GLUSTERFS
    m_glusterfsService{glusterfsService}
    ,
#endif
    m_scheduler{std::make_unique<Scheduler>(bufferSchedulerWorkers)}
    , m_bufferLimits{std::move(bufferLimits)}
    , m_communicator{communicator}
{
}
#else

StorageHelperCreator::StorageHelperCreator(
#if WITH_CEPH
    asio::io_service &cephService,
#endif
    asio::io_service &dioService,
#if WITH_S3
    asio::io_service &s3Service,
#endif
#if WITH_SWIFT
    asio::io_service &swiftService,
#endif
#if WITH_GLUSTERFS
    asio::io_service &glusterfsService,
#endif
    std::size_t bufferSchedulerWorkers, buffering::BufferLimits bufferLimits)
    :
#if WITH_CEPH
    m_cephService{cephService}
    ,
#endif
    m_dioService{dioService}
    ,
#if WITH_S3
    m_s3Service{s3Service}
    ,
#endif
#if WITH_SWIFT
    m_swiftService{swiftService}
    ,
#endif
#if WITH_GLUSTERFS
    m_glusterfsService{glusterfsService}
    ,
#endif
    m_scheduler{std::make_unique<Scheduler>(bufferSchedulerWorkers)}
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

#if WITH_CEPH
    if (name == CEPH_HELPER_NAME)
        helper = CephHelperFactory{m_cephService}.createStorageHelper(args);
#endif

#ifdef BUILD_PROXY_IO
    if (name == PROXY_HELPER_NAME)
        helper = ProxyHelperFactory{m_communicator}.createStorageHelper(args);
#endif

#if WITH_S3
    if (name == S3_HELPER_NAME)
        helper = S3HelperFactory{m_s3Service}.createStorageHelper(args);
#endif

#if WITH_SWIFT
    if (name == SWIFT_HELPER_NAME)
        helper = SwiftHelperFactory{m_swiftService}.createStorageHelper(args);
#endif

#if WITH_GLUSTERFS
    if (name == GLUSTERFS_HELPER_NAME)
        helper = GlusterFSHelperFactory{m_glusterfsService}.createStorageHelper(args);
#endif

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
