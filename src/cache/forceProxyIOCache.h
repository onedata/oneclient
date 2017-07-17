/**
 * @file forceProxyIOCache.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FORCE_CLUSTER_PROXY_CACHE_H
#define ONECLIENT_FORCE_CLUSTER_PROXY_CACHE_H

#include "fsSubscriptions.h"

#include <folly/FBString.h>

#include <tbb/concurrent_hash_map.h>

namespace one {
namespace client {
namespace cache {

struct StdHashCompare {
    bool equal(const folly::fbstring &a, const folly::fbstring &b) const
    {
        return a == b;
    }
    std::size_t hash(const folly::fbstring &a) const
    {
        return std::hash<folly::fbstring>()(a);
    }
};

/**
 * @c ForceProxyIOCache is responsible for holding uuids of files that
 * require forcing cluster proxy during read and write operations
 */
class ForceProxyIOCache {
public:
    /**
     * Checks if file is cached.
     * @param fileUuid Uuid of file to be checked.
     */
    bool contains(const folly::fbstring &fileUuid);

    /**
     * Adds file to the cache.
     * @param fileUuid of a file to be added to the cache.
     */
    void add(const folly::fbstring &fileUuid);

    /**
     * Removes file from the cache.
     * @param fileUuid of a file to be removed from the cache.
     */
    void remove(const folly::fbstring &fileUuid);

    /**
     * Sets a callback that will be called before a file is added to the cache.
     * @param cb The callback which takes uuid as parameter.
     */
    void onAdd(std::function<void(const folly::fbstring &)> cb);

    /**
     * Sets a callback that will be called after a file is removed from the
     * cache.
     * @param cb The callback which takes uuid as parameter.
     */
    void onRemove(std::function<void(const folly::fbstring &)> cb);

private:
    std::function<void(const folly::fbstring &)> m_onAdd = [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onRemove = [](auto &) {};
    tbb::concurrent_hash_map<folly::fbstring, bool, StdHashCompare> m_cache;

    using CacheAcc = typename decltype(m_cache)::accessor;
    using ConstCacheAcc = typename decltype(m_cache)::const_accessor;
};

} // namespace cache
} // namespace client
} // namespace one

#endif // ONECLIENT_FORCE_CLUSTER_PROXY_CACHE_H
