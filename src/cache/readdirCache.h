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
#include <folly/SharedMutex.h>
#include <folly/concurrency/ConcurrentHashMap.h>
#include <folly/futures/Future.h>
#include <folly/futures/SharedPromise.h>
#include <fuse/fuse_lowlevel.h>

#include <chrono>
#include <set>

namespace one {
namespace client {
namespace cache {

using namespace std::literals;

/**
 * DirCacheEntry stores the list of entries fetched from the
 * provider for a specific directory entry.
 */
struct DirCacheEntry {
    std::chrono::system_clock::time_point ctime;
    std::atomic_ullong atime;
    std::atomic_bool invalid;
    std::list<folly::fbstring> dirEntries;

    DirCacheEntry() = default;
    ~DirCacheEntry() = default;
    DirCacheEntry(const DirCacheEntry &e);
    DirCacheEntry(DirCacheEntry &&e);

    const auto &cDirEntries() const;

    bool isValid(std::chrono::milliseconds duration);

    void invalidate();

    void touch();
};

/**
 * This class provides a cache for readdir entries which can be fetched
 * from Oneprovider in much larger chunks than is allowed by the Fuse
 * page limit, and then retreived by Fuse from this cache in smaller chunks.
 */
class ReaddirCache {
public:
    ReaddirCache(
        LRUMetadataCache &metadataCache, std::shared_ptr<Context> context);

    virtual ~ReaddirCache() {}

    folly::fbvector<folly::fbstring> readdir(const folly::fbstring &uuid,
        const off_t off, const std::size_t chunkSize);

    void invalidate(const folly::fbstring &uuid);

private:
    template <typename SrvMsg, typename CliMsg>
    SrvMsg communicate(CliMsg &&msg, const std::chrono::seconds timeout);

    void fetch(const folly::fbstring &uuid);

    folly::ConcurrentHashMap<folly::fbstring,
        std::shared_ptr<folly::SharedPromise<DirCacheEntry>>>
        m_cache;

    folly::SharedMutex m_cacheMutex;

    LRUMetadataCache &m_metadataCache;

    std::shared_ptr<Context> m_context;

    const std::chrono::seconds m_providerTimeout;

    const std::size_t m_prefetchSize;

    const std::chrono::milliseconds m_cacheValidityPeriod = 2000ms;
};
}
}
}
