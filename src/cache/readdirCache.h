/**
 * @file readdirCache.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "scheduler.h"

#include "cache/lruMetadataCache.h"
#include "context.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Optional.h>
#include <folly/futures/Future.h>
#include <folly/futures/SharedPromise.h>
#include <fuse/fuse_lowlevel.h>

#include <chrono>
#include <list>

namespace one {
namespace client {
namespace cache {

using namespace std::literals;

constexpr auto READDIR_CACHE_VALIDITY_DURATION = 2000ms;

/**
 * DirCacheEntry stores the list of entries fetched from the
 * provider for a specific directory entry.
 */
class DirCacheEntry {
public:
    DirCacheEntry();
    ~DirCacheEntry() = default;
    DirCacheEntry(const DirCacheEntry &e);
    DirCacheEntry(DirCacheEntry &&e);

    /**
     * Add directory entry to cache.
     *
     * @param name Directory entry name.
     */
    void addEntry(const folly::fbstring &name);
    void addEntry(folly::fbstring &&name);

    /**
     * Returns const reference to the directory entries.
     */
    const std::list<folly::fbstring> &dirEntries() const;

    /**
     * Checks if the dir cache entry is still valid.
     */
    bool isValid(std::chrono::milliseconds duration);

    /**
     * Invalidates the cache forcibly.
     */
    void invalidate();

    /**
     * Makes the cache entry fresh again.
     */
    void touch();

    /**
     * Remove any duplicates from the directory entries container.
     */
    void unique();

private:
    /**
     * Last access time to this cache entry, stored as milliseconds
     * since epoch for atomic access.
     */
    std::atomic_ullong m_atime;

    /**
     * When true, this directory cache is invalid and should be purged.
     */
    std::atomic_bool m_invalid;

    /**
     * The directory entries list doesn't have to be locked for now as
     * it is only filled by a single thread and cannot be accessed
     * by other threads until is completely fetched from the provider.
     */
    std::list<folly::fbstring> m_dirEntries;
};

/**
 * This class provides a cache for readdir entries which can be fetched
 * from Oneprovider in much larger chunks than is allowed by the Fuse
 * page limit, and then retreived by Fuse from this cache in smaller chunks.
 */
class ReaddirCache : public std::enable_shared_from_this<ReaddirCache> {
public:
    /**
     * Constructor.
     *
     * @param metadataCache Reference to the metadata cache.
     * @param context Pointer to @c Context to access options and scheduler.
     */
    ReaddirCache(
        LRUMetadataCache &metadataCache, std::shared_ptr<Context> context);

    /**
     * Destructor
     */
    virtual ~ReaddirCache() {}

    /**
     * Read directory entries from cache or if not available fetch from server.
     *
     * @param uuid Directory id.
     * @param off Directory entry offset.
     * @param chunkSize Maximum number of directory entries to be returned.
     * @return Directory entries in the requested range.
     */
    folly::fbvector<folly::fbstring> readdir(const folly::fbstring &uuid,
        const off_t off, const std::size_t chunkSize);

    /**
     * Invalidate cache for a specific directory.
     */
    void invalidate(const folly::fbstring &uuid);

    /**
     * Returns true if cache doesn't contain any elements.
     */
    bool empty();

private:
    /**
     * Convenience template to access provider communication stack.
     */
    template <typename SrvMsg, typename CliMsg>
    SrvMsg communicate(CliMsg &&msg, const std::chrono::seconds timeout);

    /**
     * Fetch directory entries for directory 'uuid' and store them in cache.
     *
     * @param uuid Directory id.
     */
    void fetch(const folly::fbstring &uuid);

    /**
     * Removes element cache for specific directory.
     */
    void purge(const folly::fbstring &uuid);

    /**
     * Worker function which should be scheduled to clean directory entry after
     * it becomes stale. It should also schedule itself again in case the entry
     * is still valid.
     */
    void purgeWorker(
        folly::fbstring uuid, std::shared_ptr<DirCacheEntry> entry);

    /**
     * Directory entry cache.
     *
     * Each directory entry is stored in a shared promise, so that when
     * several threads try to fetch directory entries in the same time, only
     * one request to the provider is performed. The first thread will
     * initiate the fetch from the provider, and the consecutive ones will
     * wait on the future generated from this promise.
     */
    std::unordered_map<folly::fbstring,
        std::shared_ptr<folly::SharedPromise<std::shared_ptr<DirCacheEntry>>>>
        m_cache;
    std::mutex m_cacheMutex;

    /**
     * Reference to metadata cache.
     *
     * Directory entries are fetched automatically with attributes, and
     * the attributes are automatically stored in the metadatacache so that
     * they are immediately accessible to the client after listing is complete.
     */
    LRUMetadataCache &m_metadataCache;

    /**
     * Pointer to Oneclient context, used to access options and scheduler.
     */
    std::shared_ptr<Context> m_context;

    /**
     * Timeout for communication with provider.
     */
    const std::chrono::seconds m_providerTimeout;

    /**
     * The number of entries in which the 'fetch' method should request
     * the directory entries from the provider.
     */
    const std::size_t m_prefetchSize;

    /**
     * Validity period of dir cache entries.
     *
     * If a given entry has not been access in this amount of time,
     * the entry is invalid and has to retrieved again.
     */
    const std::chrono::milliseconds m_cacheValidityPeriod =
        READDIR_CACHE_VALIDITY_DURATION;
};
}
}
}
