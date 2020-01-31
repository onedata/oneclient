/**
 * @file metadataCache.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_METADATA_CACHE_H
#define ONECLIENT_METADATA_CACHE_H

#include "attrs.h"
#include "communication/communicator.h"
#include "helpers/storageHelper.h"
#include "messages/fuse/fileBlock.h"

#include <boost/icl/interval_set.hpp>
#include <boost/multi_index/composite_key.hpp>
#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index_container.hpp>
#include <folly/FBString.h>
#include <folly/Optional.h>
#include <folly/futures/Future.h>

#include <chrono>
#include <memory>
#include <vector>

namespace one {

namespace messages {
namespace fuse {
class FileRenamedEntry;
class UpdateTimes;
} // namespace fuse
} // namespace messages

namespace client {
namespace cache {

class ReaddirCache;

namespace bmi = boost::multi_index;

/**
 * @c MetadataCache is responsible for retrieving and caching file attributes
 * and locations.
 */
class MetadataCache {
public:
    MetadataCache(communication::Communicator &communicator,
        const std::chrono::seconds providerTimeout, folly::fbstring rootUuid,
        const std::vector<std::string> &spaceNames,
        const std::vector<std::string> &spaceIds);

    /**
     * Sets a pointer to an instance of @c ReaddirCache.
     *
     * @param readdirCache Shared pointer to an instance of @c ReaddirCache.
     */
    void setReaddirCache(std::shared_ptr<ReaddirCache> readdirCache);

    /**
     * Invalidates (i.e. removes from cache) direct children of directory with
     * specified uuid.
     *
     * @param uuid UUID of the parent whose direct children will be removed from
     * the cache
     */
    void invalidateChildren(const folly::fbstring &uuid);

    /**
     * Read directory entries from cache or if the directory contents are
     * currently cached by metadata cache.
     *
     * @param uuid Directory id.
     * @param off Directory entry offset.
     * @param chunkSize Maximum number of directory entries to be returned.
     * @returns Directory entries in the requested range.
     */
    folly::fbvector<folly::fbstring> readdir(
        const folly::fbstring &uuid, off_t off, std::size_t chunkSize);
    /**
     * Retrieves file attributes by uuid.
     * @param uuid Uuid of the file.
     * @returns Attributes of the file.
     */
    std::shared_ptr<FileAttr> getAttr(const folly::fbstring &uuid);

    /**
     * Retrieves file attributes by parent's uuid and file name.
     * @param parentUuid Uuid of the parent directory.
     * @param name Name of the file.
     * @returns Attributes of the file.
     */
    FileAttrPtr getAttr(
        const folly::fbstring &parentUuid, const folly::fbstring &name);

    /**
     * Inserts an externally fetched file attributes into the cache.
     * @param attr The file attributes to put in the cache.
     * @returns True, if the attribute was not in the cache
     */
    bool putAttr(std::shared_ptr<FileAttr> attr);

    /**
     * Inserts an externally fetched file location into the cache.
     * If the file has no cached attributes, they are first fetched from the
     * server.
     * @param location The location to put in the cache.
     */
    void putLocation(std::unique_ptr<FileLocation> location);

    /**
     * Returns a pointer to fetched or cached file location.
     * @param uuid Uuid of the file for which location map should be returned.
     * @param forceUpdate If true, file location will be retrieved from the
     *                    server even if cached.
     * @returns Requested file location.
     */
    std::shared_ptr<FileLocation> getLocation(
        const folly::fbstring &uuid, bool forceUpdate = false);

    /**
     * Ensures that file attributes and location is present in the cache by
     * fetching them from the server if missing.
     * @param uuid Uuid of the file.
     */
    void ensureAttrAndLocationCached(folly::fbstring uuid);

    /**
     * Removes all of file's metadata from the cache.
     * @param uuid Uuid of the file.
     */
    void erase(folly::fbstring uuid);

    /**
     * Truncates blocks in cached file locations and modifies attributes to set
     * the new size.
     * @param uuid Uuid of the file.
     * @param newSize Size to truncate to.
     */
    void truncate(folly::fbstring uuid, const std::size_t newSize);

    /**
     * Update times cached in file's attributes.
     * @param uuid Uuid of the file.
     * @param updateTimes Object containing new times.
     */
    void updateTimes(
        folly::fbstring uuid, const messages::fuse::UpdateTimes &updateTimes);

    /**
     * Changes mode cached in file's attributes.
     * @param uuid Uuid of the file.
     * @param newMode The new mode.
     */
    void changeMode(folly::fbstring uuid, const mode_t newMode);

    /**
     * Updates file attributes, if cached.
     * @param newAttr Updated attributes.
     * @returns true if attributes have been updated, false if they were not
     * cached.
     */
    bool updateAttr(std::shared_ptr<FileAttr> newAttr);

    /**
     * Updates file location, if cached.
     * @param newLocation Updated location.
     * @returns true if location has been updated, false if it was not cached.
     */
    bool updateLocation(const FileLocation &newLocation);

    /**
     * Updates file location, if cached, in a specified range.
     * @param start Start offset of the location update (inclusive)
     * @param end End offset of the location update (exclusive)
     * @param locationUpdate Updated location in a given range.
     * @returns true if location has been updated, false if it was not cached.
     */
    bool updateLocation(
        const off_t start, const off_t end, const FileLocation &locationUpdate);

    /**
     * Marks file as deleted, preventing it from being looked up by parent.
     * @param uuid Uuid of the file.
     * @returns true if file has been marked as deleted, false if it was not
     * cached.
     */
    bool markDeleted(folly::fbstring uuid);

    /**
     * Renames a cached file.
     * @param uuid Curent uuid of the file.
     * @param newParentUuid Uuid of the file's new parent.
     * @param newName New name of the file.
     * @param newUuid New uuid of the file.
     * @returns true if file has been renamed, false if it was not cached.
     */
    bool rename(folly::fbstring uuid, folly::fbstring newParentUuid,
        folly::fbstring newName, folly::fbstring newUuid,
        bool renewSubscriptions);

    /**
     * Sets a callback that will be called after a file is added to the cache.
     * @param cb The callback that takes uuid as parameter.
     */
    void onAdd(std::function<void(const folly::fbstring &)> cb)
    {
        m_onAdd = std::move(cb);
    }

    /**
     * Sets a callback that will be called after a file is marked as deleted.
     * @param cb The callback that takes uuid as parameter.
     */
    void onMarkDeleted(std::function<void(const folly::fbstring &)> cb)
    {
        m_onMarkDeleted = std::move(cb);
    }

    /**
     * Sets a callback that will be called after a file is renamed.
     * @param cb The callback which takes uuid and newUuid as parameters.
     */
    void onRename(
        std::function<void(const folly::fbstring &, const folly::fbstring &)>
            cb)
    {
        m_onRename = std::move(cb);
    }

    /**
     * Returns the current size of the metadata cache size
     */
    std::size_t size() const { return m_cache.size(); }

    /**
     * Returns true if the cache currently contains an entry with the specified
     * uuid. If not, the entry is not fetched from the server.
     *
     * @param uuid UUID of the entry to be found in the cache
     * @returns True, if the cache currently contains entry with specifed uuid
     */
    bool contains(const folly::fbstring &uuid) const;

    /**
     * Checks if the uuid points to a deleted file.
     *
     * @param uuid UUID of the entry to be found in the cache
     * @returns True, if the file has been deleted already
     */
    bool isDeleted(const folly::fbstring &uuid) const;

protected:
    /**
     * Checks if a space with a given name is whitelisted.
     */
    bool isSpaceWhitelisted(const FileAttr &space);

    struct Metadata {
        Metadata(std::shared_ptr<FileAttr>);
        std::shared_ptr<FileAttr> attr;
        std::shared_ptr<FileLocation> location;
        bool deleted = false;
    };

    struct ByUuid {
    };

    struct ByParent {
    };

    struct ByParentName {
    };

    struct NameExtractor {
        using result_type = folly::fbstring;
        const result_type &operator()(const Metadata &m) const;
    };

    struct UuidExtractor {
        using result_type = folly::fbstring;
        const result_type &operator()(const Metadata &m) const;
    };

    struct ParentUuidExtractor {
        using result_type = folly::fbstring;
        result_type operator()(const Metadata &m) const;
    };

private:
    using UuidIndexHash = std::hash<folly::fbstring>;
    using UuidIndex =
        bmi::hashed_unique<bmi::tag<ByUuid>, UuidExtractor, UuidIndexHash>;

    using ParentIndexHash = std::hash<folly::fbstring>;
    using ParentIndex = bmi::hashed_non_unique<bmi::tag<ByParent>,
        ParentUuidExtractor, ParentIndexHash>;

    using ParentNameIndexHash =
        bmi::composite_key_hash<std::hash<folly::fbstring>,
            std::hash<folly::fbstring>>;

    using ParentNameIndex = bmi::hashed_unique<bmi::tag<ByParentName>,
        bmi::composite_key<Metadata, ParentUuidExtractor, NameExtractor>,
        ParentNameIndexHash>;

    using Map = bmi::multi_index_container<Metadata,
        bmi::indexed_by<UuidIndex, ParentIndex, ParentNameIndex>>;

    Map::iterator getAttrIt(const folly::fbstring &uuid);

    template <typename ReqMsg> Map::iterator fetchAttr(ReqMsg &&msg);

    std::shared_ptr<FileLocation> getLocationPtr(
        const Map::iterator &it, bool forceUpdate = false);

    std::shared_ptr<FileLocation> fetchFileLocation(
        const folly::fbstring &uuid);

    void markDeletedIt(const Map::iterator &it);

    communication::Communicator &m_communicator;

protected:
    Map m_cache;

    std::shared_ptr<FileLocation> getLocation(std::shared_ptr<FileAttr> attr);

private:
    // This set holds UUID's of all files or directories removed in the
    // oneclient session. This is necessary to ensure that delayed
    // FileAttrChanged events about deleted files are not treated as
    // notifications about new files
    std::set<folly::fbstring> m_deletedUuids;

    std::function<void(const folly::fbstring &)> m_onAdd = [](auto) {};
    std::function<void(const folly::fbstring &)> m_onMarkDeleted = [](auto) {};
    std::function<void(const folly::fbstring &, const folly::fbstring &)>
        m_onRename = [](auto, auto) {};

    std::shared_ptr<ReaddirCache> m_readdirCache;

    const std::chrono::seconds m_providerTimeout;

    const folly::fbstring m_rootUuid;
    std::unordered_set<folly::fbstring> m_whitelistedSpaceNames;
    std::unordered_set<folly::fbstring> m_whitelistedSpaceIds;
};

} // namespace cache
} // namespace client
} // namespace one

#endif // ONECLIENT_METADATA_CACHE_H
