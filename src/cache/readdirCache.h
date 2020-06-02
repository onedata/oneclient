/**
 * @file readdirCache.h
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "scheduler.h"

#include "cache/openFileMetadataCache.h"
#include "context.h"
#include "fslogic/fiberBound.h"

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

/**
 * This class provides a cache for readdir entries which can be fetched
 * from Oneprovider in much larger chunks than is allowed by the Fuse
 * page limit, and then retreived by Fuse from this cache in smaller chunks.
 */
class ReaddirCache : public FiberBound,
                     public std::enable_shared_from_this<ReaddirCache> {
public:
    /**
     * Constructor.
     *
     * @param metadataCache Reference to the metadata cache.
     * @param context Pointer to @c Context to access options and scheduler.
     */
    ReaddirCache(OpenFileMetadataCache &metadataCache,
        std::weak_ptr<Context> context,
        std::function<void(folly::Function<void()>)> runInFiber);

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
     * Directory fetch cache.
     *
     * Stores promises for directories which are currently being asynchronusly
     * fetched from the server in batches, so that the same directory is
     * not fetched multiple times in parallel.
     */
    std::unordered_map<folly::fbstring,
        std::shared_ptr<folly::SharedPromise<folly::Unit>>>
        m_cache;

    /**
     * Reference to metadata cache.
     *
     * Directory entries are fetched automatically with attributes, and
     * the attributes are automatically stored in the metadatacache so that
     * they are immediately accessible to the client after listing is complete.
     */
    OpenFileMetadataCache &m_metadataCache;

    /**
     * Pointer to Oneclient context, used to access options and scheduler.
     */
    std::weak_ptr<Context> m_context;

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
     * Executor enabling to schedule tasks on fslogic fiber
     */
    std::function<void(folly::Function<void()>)> m_runInFiber;
};
}
}
}
