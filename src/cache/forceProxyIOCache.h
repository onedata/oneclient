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

#include <unordered_set>

namespace one {
namespace client {
namespace cache {

/**
 * @c ForceProxyIOCache is responsible for holding uuids of files that
 * require forcing cluster proxy during read and write operations
 */
class ForceProxyIOCache {
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
    bool contains(const folly::fbstring &fileUuid);

    /**
     * Inserts fileUuid to cache
     * @param fileUuid to be inserted
     */
    void insert(folly::fbstring fileUuid);

    /**
     * Erases fileUuid from cache
     * @param fileUuid to be deleted
     */
    void erase(const folly::fbstring &fileUuid);

    /**
     * Handle an event where permissions to file changed externally.
     * @param fileUuid Uuid of the file.
     */
    void handlePermissionChanged(const folly::fbstring &fileUuid);

private:
    std::unordered_set<folly::fbstring> m_cache;
    FsSubscriptions &m_fsSubscriptions;
};

} // namespace cache
} // namespace client
} // namespace one

#endif // ONECLIENT_FORCE_CLUSTER_PROXY_CACHE_H
