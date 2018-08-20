/**
 * @file fuseFileHandle.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "cache/lruMetadataCache.h"
#include "communication/communicator.h"
#include "helpers/storageHelper.h"

#include <folly/EvictingCacheMap.h>
#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Hash.h>
#include <folly/Optional.h>
#include <folly/Synchronized.h>
#include <folly/futures/Future.h>

#include <unordered_map>

namespace one {
namespace client {

namespace cache {
class HelpersCache;
class ForceProxyIOCache;
} // namespace cache

namespace fslogic {

/**
 * @c FuseFileHandle is responsible for storing information about open files.
 */
class FuseFileHandle {
public:
    /**
     * Constructor.
     * @param flags Open flags mask.
     * @param handleId A handleID representing the handle on the server.
     * @param openFileToken A token that holds the file's metadata in cache.
     * @param helpersCache Cache from which helper objects can be fetched.
     * @param forceProxyIOCache Cache for determining whether ProxyIO is forced
     * for a file.
     * @param providerTimeout Timeout for provider connections.
     */
    FuseFileHandle(const int flags, folly::fbstring handleId,
        std::shared_ptr<cache::LRUMetadataCache::OpenFileToken> openFileToken,
        cache::HelpersCache &helpersCache,
        cache::ForceProxyIOCache &forceProxyIOCache,
        std::chrono::seconds providerTimeout);

    /**
     * Retrieves a helper handle for an open file.
     * @param uuid Uuid of the file.
     * @param spaceId Id of the space for which the helper should be returned.
     * @param storageId ID of the storage of the file.
     * @param fileId ID of a file on the storage.
     * @returns A new or cached file handle for the location.
     */
    helpers::FileHandlePtr getHelperHandle(const folly::fbstring &uuid,
        const folly::fbstring &spaceId, const folly::fbstring &storageId,
        const folly::fbstring &fileId);

    /**
     * Releases an open helper handle for a file.
     * @todo @c getHelperHandle could return a releaseable object instead that
     * would wrap a @c FileHandlePtr .
     * @param uuid Uuid of the file.
     * @param storageId ID of the storage of the file.
     * @param fileId ID of a file on the storage.
     */
    void releaseHelperHandle(const folly::fbstring &uuid,
        const folly::fbstring &storageId, const folly::fbstring &fileId);

    /**
     * @returns Open flags with which the handle was created.
     */
    int flags() const { return m_flags; }

    /**
     * @returns All helper file handles open in this handle.
     */
    folly::fbvector<helpers::FileHandlePtr> helperHandles() const;

    /**
     * @returns A handleID representing the handle on the server.
     */
    folly::Optional<folly::fbstring> providerHandleId() const;

    void setLastPrefetch(boost::icl::discrete_interval<off_t> p)
    {
        m_lastPrefetch = p;
    }

    boost::icl::discrete_interval<off_t> lastPrefetch() const
    {
        return m_lastPrefetch;
    }

    bool fullPrefetchTriggered() const { return m_fullPrefetchTriggered; }

    void setFullPrefetchTriggered() { m_fullPrefetchTriggered = true; }

    bool prefetchAlreadyRequestedAt(off_t offset) const;

    void addPrefetchAt(off_t offset);

private:
    std::unordered_map<folly::fbstring, folly::fbstring> makeParameters(
        const folly::fbstring &uuid);

    const int m_flags;
    folly::fbstring m_handleId;
    std::shared_ptr<cache::LRUMetadataCache::OpenFileToken> m_openFileToken;
    cache::HelpersCache &m_helpersCache;
    cache::ForceProxyIOCache &m_forceProxyIOCache;
    std::unordered_map<std::tuple<folly::fbstring, folly::fbstring, bool>,
        helpers::FileHandlePtr>
        m_helperHandles;
    const std::chrono::seconds m_providerTimeout;
    boost::icl::discrete_interval<off_t> m_lastPrefetch;
    std::atomic<bool> m_fullPrefetchTriggered;

    folly::Synchronized<folly::EvictingCacheMap<off_t, bool>>
        m_recentPrefetchOffsets;
};

} // namespace fslogic
} // namespace client
} // namespace one
