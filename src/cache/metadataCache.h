/**
 * @file metadataCache.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_METADATA_CACHE_H
#define ONECLIENT_METADATA_CACHE_H

#include "communication/communicator.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"

#include <boost/filesystem/path.hpp>
#include <condition_variable>
#include <tbb/concurrent_hash_map.h>

namespace one {
namespace client {

/**
 * @c MetadataCache is responsible for retrieving and caching path<->uuid,
 * uuid<->fileAttrs, and uuid<->fileLocation mappings.
 */
class MetadataCache {
public:
    using Path = boost::filesystem::path;
    using FileAttr = messages::fuse::FileAttr;
    using FileLocation = messages::fuse::FileLocation;

    /**
     * @c Metadata holds metadata of a file.
     */
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
    tbb::concurrent_hash_map<std::string,
        std::pair<std::unique_ptr<std::mutex>,
                                 std::unique_ptr<std::condition_variable_any>>>
        m_mutexConditionPairMap;

public:
    using ConstUuidAccessor = decltype(m_pathToUuid)::const_accessor;
    using ConstMetaAccessor = decltype(m_metaCache)::const_accessor;
    using ConstMutexAccessor =
        decltype(m_mutexConditionPairMap)::const_accessor;
    using UuidAccessor = decltype(m_pathToUuid)::accessor;
    using MetaAccessor = decltype(m_metaCache)::accessor;
    using MutexAccessor = decltype(m_mutexConditionPairMap)::accessor;

    /**
     * Constructor.
     * @param communicator Communicator instance used for fetching missing
     * data.
     */
    MetadataCache(communication::Communicator &communicator);

    /**
     * Sets metadata accessor for a given uuid, without consulting the remote
     * endpoint in any case.
     * @param metaAcc Metadata accessor.
     * @param uuid UUID of the file to retrieve metadata of.
     * @return True if metadata has been found in the cache, false otherwise.
     */
    bool get(MetaAccessor &metaAcc, const std::string &uuid);

    /**
     * Retrieves attributes for a given path.
     * @param path The path of a file to retrieve attributes of.
     * @return Attributes of the file.
     */
    FileAttr getAttr(const Path &path);

    /**
     * Retrieves attributes of a file with given uuid.
     * @param uuid The uuid of a file to retrieve attributes of.
     * @return Attributes of the file.
     */
    FileAttr getAttr(const std::string &uuid);

    /**
     * Retrieves location data about a file with given uuid.
     * @param uuid The uuid of a file to retrieve location data about.
     * @return Location data about the file.
     */
    FileLocation getLocation(const std::string &uuid);

    /**
     * Retrieves mutex and condition assigned to file with given uuid. Creates
     * them, if they are not found.
     * @param uuid The uuid of a file to get mutex and condition.
     * @return Pair: mutex, condition.
     */
    std::pair<std::mutex &, std::condition_variable_any &>
    getMutexConditionPair(const std::string &uuid);

    /**
     * Sets metadata accessor for a given path, first ensuring that path<->UUID
     * mapping is present in the cache and attributes are set.
     * @param metaAcc Metadata accessor.
     * @param path The path of a file to retrieve attributes of.
     */
    void getAttr(MetaAccessor &metaAcc, const Path &path);

    /**
     * Sets UUID and metadata accessors for a given path, first ensuring that
     * path<->UUID mapping is present in the cache and attributes are set in
     * the metadata.
     * @param uuidAcc UUID accessor.
     * @param metaAcc Metadata accessor.
     * @param path The path of a file to retrieve attributes of.
     */
    void getAttr(
        UuidAccessor &uuidAcc, MetaAccessor &metaAcc, const Path &path);

    /**
     * Sets metadata accessor for a given UUID, first ensuring that attributes
     * are set.
     * @param metaAcc Metadata accessor.
     * @param uuid The UUID of a file to retrieve attributes of.
     */
    void getAttr(MetaAccessor &metaAcc, const std::string &uuid);

    /**
     * Sets metadata accessor for a given UUID, first ensuring that location
     * data is set.
     * @param metaAcc Metadata accessor.
     * @param uuid The UUID of a file to retrieve location data about.
     */
    void getLocation(MetaAccessor &metaAcc, const std::string &uuid);

    /**
     * Adds an arbitrary path<->UUID mapping to the cache.
     * @param path The path of the mapping.
     * @param uuid The UUID of the mapping.
     */
    void map(Path path, std::string uuid);

    /**
     * Adds an arbitrary path<->UUID and UUID<->fileLocation to the cache.
     * @param path The path of the mapping.
     * @param location The location data to put in the metadata.
     */
    void map(Path path, FileLocation location);

    /**
     * Renames a file in the cache through changing mappings.
     * @param oldPath Path to rename from.
     * @param newPath Path to rename to.
     */
    void rename(const Path &oldPath, const Path &newPath);

    /**
     * Removes a UUID and metadata entries from the cache.
     * @param uuidAcc Accessor to UUID mapping to remove.
     * @param metaAcc Accessor to metadata mapping to remove.
     */
    void remove(UuidAccessor &uuidAcc, MetaAccessor &metaAcc);

    /**
     * Waits for file location update on given condition.
     * @param uuid of file
     */
    bool waitForNewLocation(const std::string &uuid,
        const boost::icl::discrete_interval<off_t> &range,
        std::unique_lock<std::mutex> &lock,
        std::condition_variable_any &condition,
        const std::chrono::milliseconds &timeout);

    /**
     * Notifies waiting processes that the new file location has arrived
     */
    void notifyNewLocationArrived(const std::string &uuid);
};

} // namespace one
} // namespace client

#endif // ONECLIENT_METADATA_CACHE_H
