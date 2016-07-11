/**
 * @file forceProxyIOCache.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FORCE_CLUSTER_PROXY_CACHE_H
#define ONECLIENT_FORCE_CLUSTER_PROXY_CACHE_H

#include <fsSubscriptions.h>
#include <shared_mutex>
#include <tbb/concurrent_unordered_set.h>

namespace one {
namespace client {

/**
 * @c ForceProxyIOCache is responsible for holding uuids of files that
 * require forcing cluster proxy during read and write operations
 */
class ForceProxyIOCache {

private:
    tbb::concurrent_unordered_set<std::string> m_cache;
    FsSubscriptions &m_fsSubscriptions;
    std::shared_timed_mutex m_cacheMutex;

public:
    /**
     * Constructor
     * @param communicator Communicator instance used to fetch helper
     * parameters.
     */
    ForceProxyIOCache(FsSubscriptions &fsSubscriptions);

    /**
     * Checks if fileUuid is present in cache
     * @param fileUuid Uuid of file to be checked.
     */
    bool contains(const std::string &fileUuid);

    /**
     * Inserts fileUuid to cache
     * @param fileUuid to be inserted
     */
    void insert(const std::string &fileUuid);

    /**
     * Erases fileUuid from cache
     * @param fileUuid to be deleted
     */
    void erase(const std::string &fileUuid);
};

} // namespace one
} // namespace client

#endif // ONECLIENT_FORCE_CLUSTER_PROXY_CACHE_H
