/**
 * @file fileContextCache.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FILE_CONTEXT_CACHE_H
#define ONECLIENT_FILE_CONTEXT_CACHE_H

#include "helpers/IStorageHelper.h"
#include "directIOHelper.h"

#include <fuse/fuse_common.h>
#include <tbb/concurrent_hash_map.h>

#include <fcntl.h>

#include <atomic>

namespace one {
namespace client {

/**
 * @c FileContextCache is responsible for holding a context for an open file.
 */
class FileContextCache {
public:
    using FuseFileHandle = decltype(std::declval<struct fuse_file_info>().fh);
    using NativeFileHandle = decltype(open("", 0));

    /**
     * @c FileContext holds context information about an open file.
     */
    struct FileContext {
        std::string uuid;
        std::shared_ptr<helpers::IStorageHelperCTX> helperCtx;
    };

private:
    tbb::concurrent_hash_map<FuseFileHandle, FileContext> m_cache;
    std::atomic<FuseFileHandle> m_nextHandle{0};

public:
    using ConstAccessor = decltype(m_cache)::const_accessor;
    using Accessor = decltype(m_cache)::accessor;

    /**
     * Retrieves a cached file context for a given file handle.
     * @param fh The file handle.
     * @return Retrieved file context.
     */
    FileContext get(FuseFileHandle fh);

    /**
     * Sets a context accessor for a given file handle.
     * @param acc Context accessor.
     * @param fh The file handle.
     */
    void get(Accessor &acc, FuseFileHandle fh);

    /**
     * Sets a context accessor for a newly created entry in the cache.
     * @param acc Context accessor.
     */
    void create(Accessor &acc);

    /**
     * Erases an entry in the cache for a given context accessor.
     * @param acc The context accessor.
     */
    void erase(Accessor &acc);
};

} // namespace one
} // namespace client

#endif // ONECLIENT_FILE_CONTEXT_CACHE_H
