/**
 * @file helpersCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpersCache.h"

#include "buffering/bufferAgent.h"
#include "messages.pb.h"
#include "messages/fuse/createStorageTestFile.h"
#include "messages/fuse/getHelperParams.h"
#include "messages/fuse/helperParams.h"
#include "messages/fuse/storageTestFile.h"
#include "messages/fuse/verifyStorageTestFile.h"

#include <folly/ThreadName.h>

#include <algorithm>
#include <chrono>
#include <functional>

namespace one {
namespace client {
namespace cache {

constexpr unsigned int VERIFY_TEST_FILE_ATTEMPTS = 5;
constexpr std::chrono::seconds VERIFY_TEST_FILE_DELAY{15};

HelpersCache::HelpersCache(communication::Communicator &communicator,
    Scheduler &scheduler, const options::Options &options)
    : m_communicator{communicator}
    , m_scheduler{scheduler}
    , m_options{options}
    , m_helpersIoService{options.getStorageHelperThreadCount()}
    , m_helperFactory
{
#if WITH_CEPH
    m_helpersIoService,
#endif
        m_helpersIoService,
#if WITH_S3
        m_helpersIoService,
#endif
#if WITH_SWIFT
        m_helpersIoService,
#endif
#if WITH_GLUSTERFS
        m_helpersIoService,
#endif
        m_helpersIoService, m_communicator,
        options.getBufferSchedulerThreadCount(),
        helpers::buffering::BufferLimits
    {
        options.getReadBufferMinSize(), options.getReadBufferMaxSize(),
            options.getReadBufferPrefetchDuration(),
            options.getWriteBufferMinSize(), options.getWriteBufferMaxSize(),
            options.getWriteBufferFlushDelay()
    }
}
, m_storageAccessManager{m_helperFactory, m_options}
{
    std::generate_n(std::back_inserter(m_helpersWorkers),
        options.getStorageHelperThreadCount(), [this] {
            return std::thread{[this] {
                folly::setThreadName("HelpersWorker");
                m_helpersIoService.run();
            }};
        });
}

HelpersCache::~HelpersCache()
{
    m_helpersIoService.stop();
    for (auto &worker : m_helpersWorkers)
        worker.join();
}

HelpersCache::HelperPtr HelpersCache::get(const folly::fbstring &fileUuid,
    const folly::fbstring &spaceId, const folly::fbstring &storageId,
    bool forceProxyIO)
{
    LOG_FCALL() << LOG_FARG(fileUuid) << LOG_FARG(storageId)
                << LOG_FARG(forceProxyIO);

    LOG_DBG(1) << "Getting storage helper for " << fileUuid << " on storage "
               << storageId;

    if (m_options.isDirectIOForced()) {
        bool accessUnset;
        std::tie(std::ignore, accessUnset) =
            m_accessType.emplace(std::make_pair(storageId, AccessType::DIRECT));

        auto helperKey = std::make_pair(storageId, false);
        auto helperIt = m_cache.find(helperKey);
        if (helperIt != m_cache.end()) {
            LOG_DBG(1) << "Found storage helper in cache for storage "
                       << storageId;
            return helperIt->second;
        }

        if (!accessUnset && m_cache.find(helperKey) == m_cache.end()) {
            LOG_DBG(1) << "Storage helper discovery already requested but not "
                          "yet available - waiting...";

            int retryCountRemaining = 10;
            constexpr int retryDelayMs = 200;

            while (retryCountRemaining--) {
                std::this_thread::sleep_for(std::chrono::milliseconds(
                    retryCountRemaining * retryDelayMs));
                if (m_cache.find(helperKey) != m_cache.end()) {
                    LOG(INFO) << "Storage helper to storage " << storageId
                              << " is now available";
                    return m_cache[helperKey];
                }
            }

            LOG(ERROR) << "Direct IO access forced to storage " << storageId
                       << " but storage is not accessible from here";
            throw std::errc::operation_not_supported;
        }

        LOG_DBG(1) << "Requesting helper parameters for storage " << storageId
                   << " in forced directIO mode";

        try {
            auto params = communication::wait(m_communicator.communicate<
                                              messages::fuse::HelperParams>(
                messages::fuse::GetHelperParams{storageId.toStdString(),
                    spaceId.toStdString(),
                    messages::fuse::GetHelperParams::HelperMode::directMode}));
            LOG_DBG(1) << "Received storage helper params: " << params.name();

            if (params.name() == helpers::PROXY_HELPER_NAME) {
                LOG(ERROR)
                    << "File " << fileUuid
                    << " is not accessible in directIO mode on this provider";
                throw std::errc::operation_not_supported;
            }
            else if (params.name() == helpers::POSIX_HELPER_NAME) {
                LOG_DBG(1)
                    << "Direct IO requested to Posix storage - attempting "
                       "storage mountpoint detection in local filesystem";

                requestStorageTestFileCreation(fileUuid, storageId);
                return get(fileUuid, spaceId, storageId, false);
            }
            else {
                LOG_DBG(1) << "Got storage helper params for file " << fileUuid
                           << " on " << params.name() << " storage "
                           << storageId;

                auto helper = m_helperFactory.getStorageHelper(
                    params.name(), params.args(), m_options.isIOBuffered());

                m_cache[std::make_pair(storageId, false)] = helper;
                return helper;
            }
        }
        catch (std::exception &e) {
            LOG_DBG(1) << "Unexpected error when waiting for storage helper: "
                       << e.what();
            throw std::errc::resource_unavailable_try_again;
        }
    }
    else {
        forceProxyIO |= m_options.isProxyIOForced();

        auto helperKey = std::make_pair(storageId, forceProxyIO);
        auto helperIt = m_cache.find(helperKey);
        if (helperIt != m_cache.end()) {
            LOG_DBG(1) << "Found storage helper in cache for storage "
                       << storageId;
            return helperIt->second;
        }

        if (!forceProxyIO) {
            decltype(m_accessType)::iterator accessTypeIt;
            bool accessUnset;

            // Check if the access type (PROXY or DIRECT) is already determined
            // for storage 'storageId'
            std::tie(accessTypeIt, accessUnset) = m_accessType.emplace(
                std::make_pair(storageId, AccessType::PROXY));

            if (accessUnset) {
                // Request identification of storage asynchronously and return
                // for now a helper in proxy mode
                accessTypeIt->second = AccessType::PROXY;
                requestStorageTestFileCreation(fileUuid, storageId);
                return get(fileUuid, spaceId, storageId, true);
            }

            if (accessTypeIt->second == AccessType::PROXY)
                return get(fileUuid, spaceId, storageId, true);
        }

        if (m_options.isDirectIOForced() && forceProxyIO) {
            LOG(ERROR) << "Direct IO access forced to storage " << storageId
                       << " but storage is not accessible from here.";
            throw std::errc::operation_not_supported;
        }

        auto params = communication::wait(
            m_communicator.communicate<messages::fuse::HelperParams>(
                messages::fuse::GetHelperParams{storageId.toStdString(),
                    spaceId.toStdString(),
                    messages::fuse::GetHelperParams::HelperMode::autoMode}));

        auto helper = m_helperFactory.getStorageHelper(
            params.name(), params.args(), m_options.isIOBuffered());

        m_cache[helperKey] = helper;
        return helper;
    }
}

void HelpersCache::requestStorageTestFileCreation(
    const folly::fbstring &fileUuid, const folly::fbstring &storageId)
{
    LOG_DBG(1) << "Requesting storage test file creation for file: '"
               << fileUuid << "' and storage: '" << storageId << "'";

    try {
        auto testFile = communication::wait(
            m_communicator.communicate<messages::fuse::StorageTestFile>(
                messages::fuse::CreateStorageTestFile{
                    fileUuid.toStdString(), storageId.toStdString()}));

        auto sharedTestFileMsg =
            std::make_shared<messages::fuse::StorageTestFile>(
                std::move(testFile));

        handleStorageTestFile(
            sharedTestFileMsg, storageId, VERIFY_TEST_FILE_ATTEMPTS);
    }
    catch (const std::system_error &e) {
        LOG(WARNING) << "Storage test file creation error, code: '" << e.code()
                     << "', message: '" << e.what() << "'";

        if (e.code().value() == EAGAIN)
            m_accessType.erase(storageId);
        else
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client.";
    }
}

void HelpersCache::handleStorageTestFile(
    std::shared_ptr<messages::fuse::StorageTestFile> testFile,
    const folly::fbstring &storageId, const std::size_t attempts)
{
    LOG_DBG(1) << "Handling storage test file for storage: '" << storageId
               << "' with remaining attempts: " << attempts << ".";

    if (attempts == 0) {
        LOG(INFO) << "Storage '" << storageId
                  << "' is not directly accessible to the client. Test "
                     "file verification attempts limit exceeded.";

        m_accessType[storageId] = AccessType::PROXY;
        return;
    }

    try {
        auto helper = m_storageAccessManager.verifyStorageTestFile(*testFile);
        if (!helper) {
            m_scheduler.schedule(
                VERIFY_TEST_FILE_DELAY, [ =, testFile = std::move(testFile) ] {
                    handleStorageTestFile(testFile, storageId, attempts - 1);
                });
            return;
        }

        auto fileContent =
            m_storageAccessManager.modifyStorageTestFile(helper, *testFile);
        requestStorageTestFileVerification(*testFile, storageId, fileContent);

        m_cache[std::make_pair(storageId, false)] = helper;
    }
    catch (const std::system_error &e) {
        LOG(ERROR) << "Storage test file handling error, code: '" << e.code()
                   << "', message: '" << e.what() << "'";

        if (e.code().value() == EAGAIN) {
            m_accessType.erase(storageId);
        }
        else {
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client.";

            m_accessType[storageId] = AccessType::PROXY;
        }
    }
}

void HelpersCache::requestStorageTestFileVerification(
    const messages::fuse::StorageTestFile &testFile,
    const folly::fbstring &storageId, const folly::fbstring &fileContent)
{
    LOG(INFO) << "Requesting verification of storage: '" << storageId
              << "' of type '" << testFile.helperParams().name();

    if (testFile.helperParams().name() == helpers::NULL_DEVICE_HELPER_NAME) {
        handleStorageTestFileVerification({}, storageId);
        return;
    }

    messages::fuse::VerifyStorageTestFile request{storageId.toStdString(),
        testFile.spaceId(), testFile.fileId(), fileContent.toStdString()};

    try {
        communication::wait(
            m_communicator.communicate<messages::fuse::FuseResponse>(
                std::move(request)));

        handleStorageTestFileVerification({}, storageId);
    }
    catch (const std::system_error &e) {
        handleStorageTestFileVerification(e.code(), {});
    }
}

void HelpersCache::handleStorageTestFileVerification(
    const std::error_code &ec, const folly::fbstring &storageId)
{
    LOG_DBG(1) << "Handling verification of storage direct access: "
               << storageId;

    if (!ec) {
        LOG(INFO) << "Storage " << storageId
                  << " is directly accessible to the client.";

        m_accessType[storageId] = AccessType::DIRECT;
    }
    else {
        LOG(ERROR) << "Storage test file verification error, code: '"
                   << ec.value() << "', message: '" << ec.message() << "'";

        if (ec.value() == EAGAIN) {
            m_accessType.erase(storageId);
        }
        else {
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client.";

            m_accessType[storageId] = AccessType::PROXY;
        }
    }
}

} // namespace cache
} // namespace client
} // namespace one
