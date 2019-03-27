/**
 * @file helpersCache.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_HELPERS_CACHE_H
#define ONECLIENT_HELPERS_CACHE_H

#include "communication/communicator.h"
#include "helpers/storageHelper.h"
#include "helpers/storageHelperCreator.h"
#include "options/options.h"
#include "scheduler.h"
#include "storageAccessManager.h"

#include <asio/io_service.hpp>
#include <asio/ts/executor.hpp>
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

/**
 * @c HelpersCache is responsible for creating and caching
 * @c helpers::StorageHelper instances.
 */
class HelpersCache {
public:
    using HelperPtr = std::shared_ptr<helpers::StorageHelper>;

    enum class AccessType { DIRECT, PROXY, UNKNOWN };

    /**
     * Constructor.
     * Starts an @c asio::io_service instance with one worker thread for
     * @c helpers::StorageHelperCreator.
     * @param communicator Communicator instance used to fetch helper
     * parameters.
     * @param scheduler Scheduler instance used to execute storage detection
     * operations.
     * @param options Options instance used to configure buffer limits.
     */
    HelpersCache(communication::Communicator &communicator,
        Scheduler &scheduler, const options::Options &options);

    /**
     * Destructor.
     * Stops the @c asio::io_service instance and a worker thread.
     */
    virtual ~HelpersCache();

    /**
     * Retrieves a helper instance.
     * @param fileUuid UUID of a file for which helper will be used.
     * @param spaceId SpaceId in the context of which the helper should be
     *                determined.
     * @param storageId Storage id for which to retrieve a helper.
     * @param forceProxyIO Determines whether to return a ProxyIO helper.
     * @return Retrieved future to helper instance shared pointer.
     */
    virtual folly::Future<HelpersCache::HelperPtr> get(
        const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
        const folly::fbstring &storageId, bool forceProxyIO);

    /**
     * Returns the storage access type for specific storage, if not
     * determined yet UNKNOWN value will be returned.
     */
    virtual HelpersCache::AccessType getAccessType(
        const folly::fbstring &storageId);

    folly::Future<folly::Unit> refreshHelperParameters(
        const folly::fbstring &storageId, const folly::fbstring &spaceId);

private:
    HelpersCache::HelperPtr requestStorageTestFileCreation(
        const folly::fbstring &fileUuid, const folly::fbstring &storageId,
        const int maxAttempts = VERIFY_TEST_FILE_ATTEMPTS);

    HelpersCache::HelperPtr handleStorageTestFile(
        std::shared_ptr<messages::fuse::StorageTestFile> testFile,
        const folly::fbstring &storageId,
        const int maxAttempts = VERIFY_TEST_FILE_ATTEMPTS);

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

    communication::Communicator &m_communicator;
    Scheduler &m_scheduler;
    const options::Options &m_options;

    asio::io_service m_helpersIoService;
    asio::executor_work_guard<asio::io_service::executor_type> m_idleWork{
        asio::make_work_guard(m_helpersIoService)};
    folly::fbvector<std::thread> m_helpersWorkers;
    std::shared_ptr<folly::IOThreadPoolExecutor> m_helpersIOExecutor;

    // Helper parameter values provided on the Oneclient commandline
    // which should override values received from Oneprovider
    const std::map<folly::fbstring,
        std::unordered_map<folly::fbstring, folly::fbstring>>
        m_helperParamOverrides;

    helpers::StorageHelperCreator m_helperFactory;

    // Instance of storage access manager used for performing automatic
    // storage detection
    StorageAccessManager m_storageAccessManager;

    // Store the access type flag for each storage, representing the
    // currently detected access type mode.
    std::unordered_map<folly::fbstring, AccessType> m_accessType;
    std::mutex m_accessTypeMutex;

    // Helpers are stored in a map where keys are defined using 2 values:
    //  - storageId of the storage
    //  - forceProxyIO flag
    using HelpersCacheKey = std::tuple<folly::fbstring, bool>;

    // Helpers are stored as shared promises to helpers, so that in
    // case multiple requests for the same storageId are called
    // simultanously, only one storage detection request will be performed
    // and the other requests will wait for fulfillment of the future
    std::unordered_map<HelpersCacheKey,
        std::shared_ptr<folly::SharedPromise<HelperPtr>>>
        m_cache;
    std::mutex m_cacheMutex;

    // Timeout for Oneprovider responses
    std::chrono::milliseconds m_providerTimeout;
};

} // namespace cache
} // namespace client
} // namespace one

#endif // ONECLIENT_HELPERS_CACHE_H
