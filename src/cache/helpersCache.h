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

#include <asio/io_service.hpp>
#include <asio/executor_work.hpp>
#include <tbb/concurrent_hash_map.h>

#include <thread>
#include <tuple>
#include <utility>

namespace one {
namespace client {

/**
 * @c HelpersCache is responsible for creating and caching
 * @c helpers::IStorageHelper instances.
 */
class HelpersCache {
public:
    using HelperPtr = std::shared_ptr<helpers::IStorageHelper>;

private:
    communication::Communicator &m_communicator;
    asio::io_service m_ioService{1};
    asio::executor_work<asio::io_service::executor_type> m_work =
        asio::make_work(m_ioService);

    std::thread m_thread;

    helpers::StorageHelperFactory m_helperFactory{
        m_ioService, m_ioService, m_communicator};

    struct HashCompare {
        bool equal(const std::tuple<std::string, bool> &j,
            const std::tuple<std::string, bool> &k) const;
        size_t hash(const std::tuple<std::string, bool> &k) const;
    };

    tbb::concurrent_hash_map<std::tuple<std::string, bool>,
        HelperPtr, HashCompare> m_cache;

public:
    using ConstAccessor = decltype(m_cache)::const_accessor;
    using Accessor = decltype(m_cache)::accessor;

    /**
     * Constructor.
     * Starts an @c asio::io_service instance with one worker thread for
     * @c helpers::StorageHelperFactory .
     * @param communicator Communicator instance used to fetch helper
     * parameters.
     */
    HelpersCache(communication::Communicator &communicator);

    /**
     * Destructor.
     * Stops the @c asio::io_service instance and a worker thread.
     */
    ~HelpersCache();

    /**
     * Retrieves a helper instance.
     * @param spaceId Space id for which to retrieve a helper.
     * @param storageId Storage id for which to retrieve a helper.
     * @param forceClusterProxy Determines whether to return a ClusterProxy
     * helper.
     * @return Retrieved helper instance.
     */
    HelperPtr get(const std::string &storageId, const bool forceClusterProxy = false);
};

} // namespace one
} // namespace client

#endif // ONECLIENT_HELPERS_CACHE_H
