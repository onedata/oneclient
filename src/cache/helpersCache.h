/**
 * @file helpersCache.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_HELPERS_CACHE_H
#define ONECLIENT_HELPERS_CACHE_H

#include "buffering/bufferAgent.h"
#include "communication/communicator.h"
#include "helpers/storageHelper.h"
#include "helpers/storageHelperCreator.h"
#include "messages/fuse/createStorageTestFile.h"
#include "messages/fuse/getHelperParams.h"
#include "messages/fuse/helperParams.h"
#include "options/options.h"
#include "scheduler.h"
#include "storageAccessManager.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Hash.h>
#include <folly/executors/IOThreadPoolExecutor.h>
#include <folly/futures/Future.h>
#include <folly/futures/SharedPromise.h>

#include <thread>
#include <tuple>
#include <utility>

namespace one {
namespace messages {
namespace fuse {
class StorageTestFile;
}
}
namespace client {
namespace options {
class Options;
}
namespace cache {

constexpr unsigned int VERIFY_TEST_FILE_ATTEMPTS = 5;
constexpr std::chrono::seconds VERIFY_TEST_FILE_DELAY{5};

class HelpersCacheBase {
public:
    using HelperPtr = std::shared_ptr<helpers::StorageHelper>;

    enum class AccessType { DIRECT, PROXY, UNKNOWN };

    /**
     * Destructor.
     */
    virtual ~HelpersCacheBase() = default;

    /**
     * Retrieves a helper instance.
     * @param fileUuid UUID of a file for which helper will be used.
     * @param spaceId SpaceId in the context of which the helper should be
     *                determined.
     * @param storageId Storage id for which to retrieve a helper.
     * @param forceProxyIO Determines whether to return a ProxyIO helper.
     * @return Retrieved future to helper instance shared pointer.
     */
    virtual folly::Future<HelpersCacheBase::HelperPtr> get(
        const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
        const folly::fbstring &storageId, bool forceProxyIO,
        bool proxyFallback) = 0;

    /**
     * Returns the storage access type for specific storage, if not
     * determined yet UNKNOWN value will be returned.
     */
    virtual HelpersCacheBase::AccessType getAccessType(
        const folly::fbstring &storageId) = 0;

    virtual folly::Future<folly::Unit> refreshHelperParameters(
        const folly::fbstring &storageId) = 0;
};

// TODO: Refactor to promises
class HelpersCacheThreadSafeAdapter : public HelpersCacheBase {
public:
    HelpersCacheThreadSafeAdapter() = default;

    HelpersCacheThreadSafeAdapter(std::unique_ptr<HelpersCacheBase> cache);

    void setCache(std::unique_ptr<HelpersCacheBase> cache);

    folly::Future<HelpersCacheBase::HelperPtr> get(
        const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
        const folly::fbstring &storageId, bool forceProxyIO,
        bool proxyFallback) override;

    HelpersCacheBase::AccessType getAccessType(
        const folly::fbstring &storageId) override;

    folly::Future<folly::Unit> refreshHelperParameters(
        const folly::fbstring &storageId) override;

private:
    mutable std::mutex m_cacheMutex;
    std::unique_ptr<HelpersCacheBase> m_cache;
};

/**
 * @c HelpersCache is responsible for creating and caching
 * @c helpers::StorageHelper instances.
 */
template <typename CommunicatorT> class HelpersCache : public HelpersCacheBase {
public:
    /**
     * Constructor.
     * @param communicator Communicator instance used to fetch helper
     * parameters.
     * @param scheduler Scheduler instance used to execute storage detection
     * operations.
     * @param options Options instance used to configure buffer limits.
     */
    HelpersCache(CommunicatorT &communicator,
        std::shared_ptr<Scheduler> scheduler, const options::Options &options,
        int maxAttempts = VERIFY_TEST_FILE_ATTEMPTS)
        : m_communicator{communicator}
        , m_scheduler{std::move(scheduler)}
        , m_options{options}
        , m_helpersIOExecutor{std::make_shared<folly::IOThreadPoolExecutor>(
              static_cast<int>(options.getStorageHelperThreadCount()))}
        , m_helperParamOverrides{options.getHelperOverrideParams()}
        , m_helperFactory
    {
#if WITH_CEPH
        m_helpersIOExecutor, m_helpersIOExecutor,
#endif
            m_helpersIOExecutor,
#if WITH_S3
            m_helpersIOExecutor,
#endif
#if WITH_SWIFT
            m_helpersIOExecutor,
#endif
#if WITH_GLUSTERFS
            m_helpersIOExecutor,
#endif
#if WITH_WEBDAV
            m_helpersIOExecutor,
#endif
#if WITH_XROOTD
            m_helpersIOExecutor,
#endif
#if WITH_NFS
            m_helpersIOExecutor,
#endif
            m_helpersIOExecutor, m_communicator,
            options.getBufferSchedulerThreadCount(),
            helpers::buffering::BufferLimits{options.getReadBufferMinSize(),
                options.getReadBufferMaxSize(),
                options.getReadBufferPrefetchDuration(),
                options.getWriteBufferMinSize(),
                options.getWriteBufferMaxSize(),
                options.getWriteBufferFlushDelay(),
                options::DEFAULT_PREFETCH_TARGET_LATENCY,
                options::DEFAULT_PREFETCH_POWER_BASE,
                options.getReadBuffersTotalSize(),
                options.getWriteBuffersTotalSize()},
            helpers::ExecutionContext::ONECLIENT
    }
    , m_storageAccessManager{m_helperFactory, m_options},
        m_providerTimeout{options.getProviderTimeout()},
        m_maxAttempts{maxAttempts}
    {
    }

    /**
     * Destructor.
     */
    virtual ~HelpersCache() = default;

    void onHelperCreated(std::function<void(folly::fbstring)> callback)
    {
        m_onHelperCreated = std::move(callback);
    };

    /**
     * Retrieves a helper instance.
     * @param fileUuid UUID of a file for which helper will be used.
     * @param spaceId SpaceId in the context of which the helper should be
     *                determined.
     * @param storageId Storage id for which to retrieve a helper.
     * @param forceProxyIO Determines whether to return a ProxyIO helper.
     * @return Retrieved future to helper instance shared pointer.
     */
    folly::Future<HelpersCacheBase::HelperPtr> get(
        const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
        const folly::fbstring &storageId, bool forceProxyIO,
        bool proxyFallback) override;

    /**
     * Returns the storage access type for specific storage, if not
     * determined yet UNKNOWN value will be returned.
     */
    HelpersCache::AccessType getAccessType(
        const folly::fbstring &storageId) override;

    folly::Future<folly::Unit> refreshHelperParameters(
        const folly::fbstring &storageId) override;

private:
    HelpersCache::HelperPtr requestStorageTestFileCreation(
        const folly::fbstring &fileUuid, const folly::fbstring &storageId,
        const int maxAttempts);

    HelpersCache::HelperPtr handleStorageTestFile(
        std::shared_ptr<messages::fuse::StorageTestFile> testFile,
        const folly::fbstring &storageId, const int maxAttempts);

    void requestStorageTestFileVerification(
        const messages::fuse::StorageTestFile &testFile,
        const folly::fbstring &storageId, const folly::fbstring &fileContent);

    void handleStorageTestFileVerification(
        const std::error_code &ec, const folly::fbstring &storageId);

    HelpersCache::HelperPtr performAutoIOStorageDetection(
        const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
        const folly::fbstring &storageId, bool forceProxyIO);

    HelpersCache::HelperPtr performForcedDirectIOStorageDetection(
        const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
        const folly::fbstring &storageId);

    folly::Future<folly::Unit> refreshHelperParameters(
        const folly::fbstring &storageId, const folly::fbstring &spaceId);

    CommunicatorT &m_communicator;
    std::shared_ptr<Scheduler> m_scheduler;
    const options::Options &m_options;

    std::shared_ptr<folly::IOThreadPoolExecutor> m_helpersIOExecutor;

    // Helper parameter values provided on the Oneclient commandline
    // which should override values received from Oneprovider
    const std::map<folly::fbstring,
        std::unordered_map<folly::fbstring, folly::fbstring>>
        m_helperParamOverrides;

    helpers::StorageHelperCreator<CommunicatorT> m_helperFactory;

    // Instance of storage access manager used for performing automatic
    // storage detection
    StorageAccessManager<CommunicatorT> m_storageAccessManager;

    // Store the access type flag for each storage, representing the
    // currently detected access type mode.
    std::unordered_map<folly::fbstring, AccessType> m_accessType;
    std::mutex m_accessTypeMutex;

    // Helpers are stored in a map where keys are defined using 2 values:
    //  - storageId of the storage
    //  - spaceId in the context of which the helper operates
    //  - forceProxyIO flag
    using HelpersCacheKey = std::tuple<folly::fbstring, folly::fbstring, bool>;

    // Helpers are stored as shared promises to helpers, so that in
    // case multiple requests for the same storageId are called
    // simultaneously, only one storage detection request will be performed
    // and the other requests will wait for fulfillment of the future
    std::unordered_map<HelpersCacheKey,
        std::shared_ptr<folly::SharedPromise<HelperPtr>>>
        m_cache;
    std::mutex m_cacheMutex;

    // Timeout for Oneprovider responses
    std::chrono::milliseconds m_providerTimeout;

    int m_maxAttempts;

    std::function<void(folly::fbstring)> m_onHelperCreated;
};

template <typename CommunicatorT>
typename HelpersCache<CommunicatorT>::AccessType
HelpersCache<CommunicatorT>::getAccessType(const folly::fbstring &storageId)
{
    std::lock_guard<std::mutex> guard(m_accessTypeMutex);

    if (m_accessType.find(storageId) == m_accessType.end())
        return AccessType::UNKNOWN;

    return m_accessType[storageId];
}

template <typename CommunicatorT>
folly::Future<folly::Unit> HelpersCache<CommunicatorT>::refreshHelperParameters(
    const folly::fbstring &storageId)
{
    LOG_FCALL() << LOG_FARG(storageId);

    std::lock_guard<std::mutex> guard(m_cacheMutex);

    folly::fbvector<folly::Future<folly::Unit>> futs;

    for (const auto &kv : m_cache) {
        if (std::get<0>(kv.first) == storageId) {
            const auto &spaceId = std::get<1>(kv.first);
            futs.emplace_back(refreshHelperParameters(storageId, spaceId));
        }
    }

    return folly::collectAll(futs)
        .via(m_helpersIOExecutor.get())
        .thenTry([](auto && /*maybe*/) { return folly::makeFuture(); });
}

template <typename CommunicatorT>
folly::Future<folly::Unit> HelpersCache<CommunicatorT>::refreshHelperParameters(
    const folly::fbstring &storageId, const folly::fbstring &spaceId)
{
    LOG_FCALL() << LOG_FARG(storageId) << LOG_FARG(spaceId);

    // Get the helper promise if exists already
    auto helperKey = std::make_tuple(storageId, spaceId, false);
    auto helperPromiseIt = m_cache.find(helperKey);

    if (helperPromiseIt == m_cache.end()) {
        LOG(WARNING) << "Trying to refresh parameters for nonexisting helper "
                        "to storage: "
                     << storageId;
        return folly::makeFuture();
    }

    // Invalidate helper parameters and obtain a new parameters promise
    return helperPromiseIt->second->getFuture().thenValue(
        [this, storageId, spaceId](HelpersCache::HelperPtr &&helper) {
            auto params = communication::wait(
                m_communicator.template communicate<messages::fuse::
                        HelperParams>(messages::fuse::GetHelperParams{
                    storageId.toStdString(), spaceId.toStdString(),
                    messages::fuse::GetHelperParams::HelperMode::directMode}),
                m_providerTimeout);

            auto paramsWithType{params.args()};
            paramsWithType["type"] = params.name();

            auto bufferedHelper =
                std::dynamic_pointer_cast<helpers::buffering::BufferAgent>(
                    helper);
            if (bufferedHelper) {
                LOG_DBG(2) << "Refreshing buffered helper " << params.name()
                           << " params for storage: " << storageId;

                return bufferedHelper->helper()->updateHelper(paramsWithType);
            }

            LOG_DBG(2) << "Refreshing helper " << params.name()
                       << " params for storage: " << storageId;

            return helper->updateHelper(paramsWithType);
        });
}

template <typename CommunicatorT>
folly::Future<typename HelpersCache<CommunicatorT>::HelperPtr>
HelpersCache<CommunicatorT>::get(const folly::fbstring &fileUuid,
    const folly::fbstring &spaceId, const folly::fbstring &storageId,
    bool forceProxyIO, bool proxyFallback)
{
    LOG_FCALL() << LOG_FARG(fileUuid) << LOG_FARG(storageId)
                << LOG_FARG(forceProxyIO);

    LOG_DBG(2) << "Getting storage helper for file " << fileUuid
               << " on storage " << storageId << " and space " << spaceId;

    if (!proxyFallback) {
        if (m_options.isDirectIOForced() && forceProxyIO) {
            LOG(ERROR) << "Direct IO and force IO options cannot be "
                          "simultaneously set.";
            throw std::errc::operation_not_supported; // NOLINT
        }

        if (m_options.isDirectIOForced()) {
            auto helperKey = std::make_tuple(storageId, spaceId, false);
            auto helperPromiseIt = m_cache.find(helperKey);

            std::lock_guard<std::mutex> guard(m_cacheMutex);

            if (helperPromiseIt == m_cache.end()) {
                LOG_DBG(2)
                    << "Storage helper promise for storage " << storageId
                    << " in direct mode unavailable - creating new storage "
                       "helper...";

                auto p = std::make_shared<folly::SharedPromise<HelperPtr>>();

                m_cache.emplace(std::make_tuple(storageId, spaceId, false), p);

                m_scheduler->post(
                    [this, &fileUuid, &spaceId, &storageId, p = std::move(p)] {
                        p->setWith([=] {
                            return performForcedDirectIOStorageDetection(
                                fileUuid, spaceId, storageId);
                        });
                    });
            }

            return m_cache.find(helperKey)->second->getFuture().thenTry(
                [this, storageId](auto &&helper) {
                    helper.throwUnlessValue();

                    if (m_onHelperCreated && helper.value().get() != nullptr &&
                        helper.value()->name() != "proxy")
                        m_onHelperCreated(storageId);
                    return helper.value();
                });
        }
    }

    forceProxyIO |= (m_options.isProxyIOForced() || proxyFallback);

    auto helperKey = std::make_tuple(storageId, spaceId, forceProxyIO);

    std::lock_guard<std::mutex> guard(m_cacheMutex);

    auto helperPromiseIt = m_cache.find(helperKey);
    if (helperPromiseIt == m_cache.end()) {
        LOG_DBG(2) << "Storage helper promise for storage " << storageId
                   << " unavailable - creating new storage helper...";

        auto p = std::make_shared<folly::SharedPromise<HelperPtr>>();

        m_cache.emplace(std::make_tuple(storageId, spaceId, forceProxyIO), p);

        m_scheduler->post([this, &fileUuid, &spaceId, &storageId, forceProxyIO,
                              p = std::move(p)] {
            p->setWith([=] {
                return performAutoIOStorageDetection(
                    fileUuid, spaceId, storageId, forceProxyIO);
            });
        });
    }

    return m_cache.find(helperKey)->second->getFuture().thenTry(
        [this, storageId](auto &&helper) {
            helper.throwUnlessValue();

            if (m_onHelperCreated && helper.value()->name() != "proxy")
                m_onHelperCreated(storageId);
            return helper.value();
        });
}

template <typename CommunicatorT>
typename HelpersCache<CommunicatorT>::HelperPtr
HelpersCache<CommunicatorT>::performAutoIOStorageDetection(
    const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
    const folly::fbstring &storageId, bool forceProxyIO)
{
    LOG(INFO)
        << "Performing automatic storage access type detection for storage "
        << storageId << " for file " << fileUuid
        << " with forced proxy io mode: " << forceProxyIO;

    bool accessUnset = false;
    auto accessTypeKey = std::make_pair(storageId, AccessType::PROXY);

    // Check if the access type (PROXY or DIRECT) is already
    // determined for storage 'storageId'
    {
        std::lock_guard<std::mutex> guard(m_accessTypeMutex);
        std::tie(std::ignore, accessUnset) =
            m_accessType.emplace(accessTypeKey);
    }

    if (!forceProxyIO) {
        if (accessUnset) {
            std::unordered_map<folly::fbstring, folly::fbstring> overrideParams;
            if (m_helperParamOverrides.find(storageId) !=
                m_helperParamOverrides.end())
                overrideParams = m_helperParamOverrides.at(storageId);

            auto params = communication::wait(
                m_communicator.template communicate<messages::fuse::
                        HelperParams>(messages::fuse::GetHelperParams{
                    storageId.toStdString(), spaceId.toStdString(),
                    messages::fuse::GetHelperParams::HelperMode::directMode}),
                m_providerTimeout);

            if (params.name() == helpers::PROXY_HELPER_NAME) {
                LOG(INFO) << "Storage " << storageId
                          << " not accessible for direct access from this "
                             "Oneprovider - switching to proxy mode.";

                return m_helperFactory.getStorageHelper(params.name(),
                    params.args(), m_options.isIOBuffered(), overrideParams);
            }

            if (params.name() == helpers::POSIX_HELPER_NAME &&
                overrideParams.find("mountPoint") != overrideParams.end()) {

                one::client::checkPosixMountpointOverride(
                    storageId, overrideParams);

                {
                    std::lock_guard<std::mutex> guard(m_accessTypeMutex);
                    auto at = m_accessType.emplace(
                        std::make_pair(storageId, AccessType::DIRECT));
                    if (!at.second)
                        at.first->second = AccessType::DIRECT;
                }

                return m_helperFactory.getStorageHelper(params.name(),
                    params.args(), m_options.isIOBuffered(), overrideParams);
            }

            // First try to quickly detect direct io (in 1 attempt), if not
            // available, return proxy and schedule full storage detection
            auto helper =
                requestStorageTestFileCreation(fileUuid, storageId, 1);
            if (helper) {
                LOG(INFO) << "Direct access to " << params.name() << " storage "
                          << storageId << " determined on first attempt";
                return helper;
            }

            LOG_DBG(2) << "Direct access to storage " << storageId
                       << " wasn't determined on first attempt - "
                          "scheduling retry and return proxy helper as "
                          "fallback";
            m_scheduler->post([this, fileUuid, storageId, spaceId,
                                  storageType = params.name()] {
                auto directIOHelper = requestStorageTestFileCreation(
                    fileUuid, storageId, m_maxAttempts);
                if (directIOHelper) {
                    LOG_DBG(2) << "Found direct access to " << storageType
                               << " storage " << storageId
                               << " using automatic storage detection";
                    {
                        std::lock_guard<std::mutex> guard(m_cacheMutex);
                        m_cache[std::make_tuple(storageId, spaceId, false)]
                            ->setValue(directIOHelper);
                    }

                    {
                        std::lock_guard<std::mutex> guard(m_accessTypeMutex);
                        auto at = m_accessType.emplace(
                            std::make_pair(storageId, AccessType::DIRECT));
                        if (!at.second)
                            at.first->second = AccessType::DIRECT;
                    }
                }
                else {
                    LOG(INFO) << "Direct access to " << storageType
                              << " storage " << storageId
                              << " couldn't be established - leaving "
                                 "proxy access";
                }
            });
            return performAutoIOStorageDetection(
                fileUuid, spaceId, storageId, true);
        }

        std::lock_guard<std::mutex> guard(m_accessTypeMutex);
        if (m_accessType[storageId] == AccessType::PROXY)
            return performAutoIOStorageDetection(
                fileUuid, spaceId, storageId, true);
    }

    auto params = communication::wait(
        m_communicator.template communicate<messages::fuse::HelperParams>(
            messages::fuse::GetHelperParams{storageId.toStdString(),
                spaceId.toStdString(),
                messages::fuse::GetHelperParams::HelperMode::proxyMode}),
        m_providerTimeout);

    std::unordered_map<folly::fbstring, folly::fbstring> overrideParams;
    if (m_helperParamOverrides.find(storageId) != m_helperParamOverrides.end())
        overrideParams = m_helperParamOverrides.at(storageId);

    return m_helperFactory.getStorageHelper(
        params.name(), params.args(), m_options.isIOBuffered(), overrideParams);
}

template <typename CommunicatorT>
typename HelpersCache<CommunicatorT>::HelperPtr
HelpersCache<CommunicatorT>::performForcedDirectIOStorageDetection(
    const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
    const folly::fbstring &storageId)
{
    LOG_DBG(1) << "Requesting helper parameters for storage " << storageId
               << " in forced direct IO mode";

    {
        std::lock_guard<std::mutex> guard(m_accessTypeMutex);
        m_accessType.emplace(std::make_pair(storageId, AccessType::DIRECT));
    }

    try {
        std::unordered_map<folly::fbstring, folly::fbstring> overrideParams;
        if (m_helperParamOverrides.find(storageId) !=
            m_helperParamOverrides.end())
            overrideParams = m_helperParamOverrides.at(storageId);

        auto params = communication::wait(
            m_communicator.template communicate<messages::fuse::HelperParams>(
                messages::fuse::GetHelperParams{storageId.toStdString(),
                    spaceId.toStdString(),
                    messages::fuse::GetHelperParams::HelperMode::directMode}),
            m_providerTimeout);

        if (params.name() == helpers::PROXY_HELPER_NAME) {
            LOG(ERROR) << "File " << fileUuid
                       << " is not accessible in direct IO mode "
                          "on this provider";
            throw std::errc::operation_not_supported; // NOLINT
        }

        if (params.name() == helpers::POSIX_HELPER_NAME &&
            overrideParams.find("mountPoint") == overrideParams.end()) {
            LOG(INFO) << "Direct IO requested to Posix storage " << storageId
                      << " - "
                         "attempting storage mountpoint detection in local "
                         "filesystem";

            return requestStorageTestFileCreation(
                fileUuid, storageId, m_maxAttempts);
        }

        LOG_DBG(1) << "Got storage helper params for file " << fileUuid
                   << " on " << params.name() << " storage " << storageId;

        one::client::checkPosixMountpointOverride(storageId, overrideParams);

        return m_helperFactory.getStorageHelper(params.name(), params.args(),
            m_options.isIOBuffered(), overrideParams);
    }
    catch (std::exception &e) {
        LOG_DBG(1) << "Unexpected error when waiting for "
                      "storage helper: "
                   << e.what();
        throw std::errc::resource_unavailable_try_again; // NOLINT
    }
}

template <typename CommunicatorT>
typename HelpersCache<CommunicatorT>::HelperPtr
HelpersCache<CommunicatorT>::requestStorageTestFileCreation(
    const folly::fbstring &fileUuid, const folly::fbstring &storageId,
    const int maxAttempts)
{
    LOG_DBG(1) << "Requesting storage test file creation for file: '"
               << fileUuid << "' and storage: '" << storageId << "'";

    try {
        auto testFile = communication::wait(
            m_communicator
                .template communicate<messages::fuse::StorageTestFile>(
                    messages::fuse::CreateStorageTestFile{
                        fileUuid.toStdString(), storageId.toStdString()}),
            m_providerTimeout);

        auto sharedTestFileMsg =
            std::make_shared<messages::fuse::StorageTestFile>(
                std::move(testFile));

        return handleStorageTestFile(sharedTestFileMsg, storageId, maxAttempts);
    }
    catch (const std::system_error &e) {
        LOG(WARNING) << "Storage test file creation error, code: '" << e.code()
                     << "', message: '" << e.what() << "'";

        if (e.code().value() == EAGAIN) {
            std::lock_guard<std::mutex> guard(m_accessTypeMutex);
            m_accessType.erase(storageId);
        }
        else
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client.";

        return {};
    }
}

template <typename CommunicatorT>
typename HelpersCache<CommunicatorT>::HelperPtr
HelpersCache<CommunicatorT>::handleStorageTestFile(
    std::shared_ptr<messages::fuse::StorageTestFile> testFile,
    const folly::fbstring &storageId, const int maxAttempts)
{
    LOG_DBG(1) << "Handling storage test file for storage: " << storageId;

    try {
        auto helper =
            m_storageAccessManager.verifyStorageTestFile(storageId, *testFile);
        auto attempts = maxAttempts;

        while (!helper && (attempts-- > 0)) {
            std::this_thread::sleep_for(VERIFY_TEST_FILE_DELAY);
            helper = m_storageAccessManager.verifyStorageTestFile(
                storageId, *testFile);
        }

        if (!helper) {
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client. Test "
                         "file verification attempts limit ("
                      << m_maxAttempts << ") exceeded.";

            std::lock_guard<std::mutex> guard(m_accessTypeMutex);
            m_accessType[storageId] = AccessType::PROXY;
            return {};
        }

        LOG_DBG(2)
            << "Got storage helper - attempting to modify storage test file";

        auto fileContent =
            one::client::modifyStorageTestFile(storageId, helper, *testFile);

        LOG_DBG(2) << "Storage test file modified with content: "
                   << fileContent;

        requestStorageTestFileVerification(*testFile, storageId, fileContent);

        return helper;
    }
    catch (const std::system_error &e) {
        LOG(ERROR) << "Storage test file handling error, code: '" << e.code()
                   << "', message: '" << e.what() << "'";

        std::lock_guard<std::mutex> guard(m_accessTypeMutex);

        if (e.code().value() == EAGAIN) {
            m_accessType.erase(storageId);
        }
        else {
            LOG(INFO) << "Storage '" << storageId
                      << "' is not directly accessible to the client.";

            m_accessType[storageId] = AccessType::PROXY;
        }

        return {};
    }
    catch (const std::exception &e) {
        LOG(ERROR) << "Storage test file handling error:  message: '"
                   << e.what() << "'";

        LOG(INFO) << "Storage '" << storageId
                  << "' is not directly accessible to the client.";

        m_accessType[storageId] = AccessType::PROXY;

        return {};
    }
}

template <typename CommunicatorT>
void HelpersCache<CommunicatorT>::requestStorageTestFileVerification(
    const messages::fuse::StorageTestFile &testFile,
    const folly::fbstring &storageId, const folly::fbstring &fileContent)
{
    LOG(INFO) << "Requesting verification of modified storage test file: '"
              << storageId << "' of type '" << testFile.helperParams().name();

    if (testFile.helperParams().name() == helpers::NULL_DEVICE_HELPER_NAME) {
        handleStorageTestFileVerification({}, storageId);
        return;
    }

    messages::fuse::VerifyStorageTestFile request{storageId.toStdString(),
        testFile.spaceId(), testFile.fileId(), fileContent.toStdString()};

    try {
        communication::wait(
            m_communicator.template communicate<messages::fuse::FuseResponse>(
                std::move(request)),
            m_providerTimeout);

        handleStorageTestFileVerification({}, storageId);
    }
    catch (const std::system_error &e) {
        handleStorageTestFileVerification(e.code(), {});
    }
}

template <typename CommunicatorT>
void HelpersCache<CommunicatorT>::handleStorageTestFileVerification(
    const std::error_code &ec, const folly::fbstring &storageId)
{
    LOG_DBG(1) << "Handling verification of storage direct access: "
               << storageId;

    std::lock_guard<std::mutex> guard(m_accessTypeMutex);

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

#endif // ONECLIENT_HELPERS_CACHE_H
