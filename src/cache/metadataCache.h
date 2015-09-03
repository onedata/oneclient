#ifndef ONECLIENT_METADATA_CACHE_H
#define ONECLIENT_METADATA_CACHE_H

#include "communication/communicator.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"

#include <boost/filesystem/path.hpp>
#include <tbb/concurrent_hash_map.h>

namespace one {
namespace client {

class MetadataCache {
public:
    using Path = boost::filesystem::path;
    using FileAttr = messages::fuse::FileAttr;
    using FileLocation = messages::fuse::FileLocation;

    struct Metadata {
        boost::optional<Path> path;
        boost::optional<FileAttr> attr;
        boost::optional<FileLocation> location;
    };

private:
    struct PathHash {
        static std::size_t hash(const Path &);
        static bool equal(const Path &, const Path &);
    };

    communication::Communicator &m_communicator;
    tbb::concurrent_hash_map<Path, std::string, PathHash> m_pathToUuid;
    tbb::concurrent_hash_map<std::string, Metadata> m_metaCache;

public:
    using ConstUuidAccessor = decltype(m_pathToUuid)::const_accessor;
    using ConstMetaAccessor = decltype(m_metaCache)::const_accessor;
    using UuidAccessor = decltype(m_pathToUuid)::accessor;
    using MetaAccessor = decltype(m_metaCache)::accessor;

    MetadataCache(communication::Communicator &communicator);

    FileAttr getAttr(const Path &path);
    FileAttr getAttr(const std::string &uuid);
    FileLocation getLocation(const std::string &uuid);

    void getAttr(
        UuidAccessor &uuidAcc, MetaAccessor &metaAcc, const Path &path);
    void getAttr(MetaAccessor &metaAcc, const std::string &uuid);
    void getLocation(MetaAccessor &metaAcc, const std::string &uuid);

    void rename(const Path &oldPath, const Path &newPath);
    void map(Path path, std::string uuid);
    void remove(UuidAccessor &uuidAcc, MetaAccessor &metaAcc);
};

} // namespace one
} // namespace client

#endif // ONECLIENT_METADATA_CACHE_H
