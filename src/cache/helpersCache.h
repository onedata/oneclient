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
#include "helpers/IStorageHelper.h"
#include "helpers/storageHelperFactory.h"
#include "proxyio/bufferAgent.h"
#include "scheduler.h"
#include "storageAccessManager.h"

#include <asio/executor_work.hpp>
#include <asio/io_service.hpp>
#include <tbb/concurrent_hash_map.h>

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

/**
 * @c HelpersCache is responsible for creating and caching
 * @c helpers::IStorageHelper instances.
 */
class HelpersCache {
public:
    using HelperPtr = std::shared_ptr<helpers::IStorageHelper>;

private:
    void requestStorageTestFileCreation(
        const std::string &fileUuid, const std::string &storageId);

    void handleStorageTestFile(
        std::shared_ptr<messages::fuse::StorageTestFile> testFile,
        const std::string &storageId, unsigned int attempts);

    void requestStorageTestFileVerification(
        const messages::fuse::StorageTestFile &testFile,
        const std::string &storageId, std::string fileContent);

    void handleStorageTestFileVerification(
        const std::error_code &ec, const std::string &storageId);

    enum class AccessType { DIRECT, PROXY };

    struct HashCompare {
        bool equal(const std::tuple<std::string, bool> &j,
            const std::tuple<std::string, bool> &k) const;
        size_t hash(const std::tuple<std::string, bool> &k) const;
    };

    communication::Communicator &m_communicator;
    Scheduler &m_scheduler;
    asio::io_service m_ioService{1};
    asio::executor_work<asio::io_service::executor_type> m_work =
        asio::make_work(m_ioService);
    std::thread m_thread;

    helpers::proxyio::BufferAgent m_bufferAgent{
        {}, m_communicator, m_scheduler};

    helpers::StorageHelperFactory m_helperFactory{
        m_ioService, m_ioService, m_ioService, m_bufferAgent};

    StorageAccessManager m_storageAccessManager;

    tbb::concurrent_hash_map<std::tuple<std::string, bool>, HelperPtr,
        HashCompare>
        m_cache;
    tbb::concurrent_hash_map<std::string, AccessType> m_accessType;

public:
    using ConstAccessTypeAccessor = decltype(m_accessType)::const_accessor;
    using AccessTypeAccessor = decltype(m_accessType)::accessor;
    using ConstCacheAccessor = decltype(m_cache)::const_accessor;
    using CacheAccessor = decltype(m_cache)::accessor;

    /**
     * Constructor.
     * Starts an @c asio::io_service instance with one worker thread for
     * @c helpers::StorageHelperFactory .
     * @param communicator Communicator instance used to fetch helper
     * parameters.
     */
    HelpersCache(
        communication::Communicator &communicator, Scheduler &scheduler);

    /**
     * Destructor.
     * Stops the @c asio::io_service instance and a worker thread.
     */
    ~HelpersCache();

    /**
     * Retrieves a helper instance.
     * @param fileUuid UUID of a file for which helper will be used.
     * @param storageId Storage id for which to retrieve a helper.
     * @param forceProxyIO Determines whether to return a ProxyIO helper.
     * @return Retrieved helper instance.
     */
    HelperPtr get(const std::string &fileUuid, const std::string &storageId,
        bool forceProxyIO = false);
};

} // namespace one
} // namespace client

#endif // ONECLIENT_HELPERS_CACHE_H
