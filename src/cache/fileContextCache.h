#ifndef ONECLIENT_FILE_CONTEXT_CACHE_H
#define ONECLIENT_FILE_CONTEXT_CACHE_H

#include "helpers/IStorageHelper.h"

#include <fuse/fuse_common.h>
#include <tbb/concurrent_hash_map.h>

#include <fcntl.h>

#include <atomic>

namespace one {
namespace client {

using FuseFileHandle = decltype(std::declval<struct fuse_file_info>().fh);
using NativeFileHandle = decltype(open("", 0));

struct FileContext {
    std::string uuid;
    helpers::StorageHelperCTX helperCtx;
};

class FileContextCache {
private:
    tbb::concurrent_hash_map<FuseFileHandle, FileContext> m_cache;
    std::atomic<FuseFileHandle> m_nextHandle{0};

public:
    using ConstAccessor = decltype(m_cache)::const_accessor;
    using Accessor = decltype(m_cache)::accessor;

    FileContext get(FuseFileHandle fh);
    void get(Accessor &acc, FuseFileHandle fh);
    void create(Accessor &acc);
    void erase(Accessor &acc);
};

} // namespace one
} // namespace client

#endif // ONECLIENT_FILE_CONTEXT_CACHE_H
