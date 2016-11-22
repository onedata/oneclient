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

/**
 * @c MetadataCache is responsible for retrieving and caching file attributes
 * and locations.
 */
class MetadataCache {
public:
    MetadataCache(communication::Communicator &communicator);

    /**
     * Retrieves file attributes by uuid.
     * @param uuid Uuid of the file.
     * @returns Attributes of the file.
     */
    FileAttrPtr getAttr(const folly::fbstring &uuid);

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
     */
    void putAttr(std::shared_ptr<FileAttr> attr);

    /**
     * Adds a specific block to a cached file locations. File location must be
     * present in the cache.
     * @param uuid Uuid of the file.
     * @param range The range of the added block.
     * @param fileBlock The block.
     */
    void addBlock(const folly::fbstring &uuid,
        const boost::icl::discrete_interval<off_t> range,
        messages::fuse::FileBlock fileBlock);

    /**
     * Retrieves a block from file locations that contains a specific
     * offset.
     * @param uuid Uuid of the file.
     * @param offset Offset to search for.
     * @returns Pair of range and block denoting the found block.
     */
    folly::Optional<std::pair<boost::icl::discrete_interval<off_t>,
        messages::fuse::FileBlock>>
    getBlock(const folly::fbstring &uuid, const off_t offset);

    /**
     * Retrieves a default block from file locations.
     * If the file has no cached attributes, they are first fetched from the
     * server.
     * @param uuid Uuid of the file.
     * @returns File block.
     */
    messages::fuse::FileBlock getDefaultBlock(const folly::fbstring &uuid);

    /**
     * Inserts an externally fetched file location into the cache.
     * If the file has no cached attributes, they are first fetched from the
     * server.
     * @param location The location to put in the cache.
     */
    void putLocation(std::unique_ptr<FileLocation> location);

    /**
     * Retrieves space Id by uuid.
     * @param uuid Uuid of the file.
     * @returns Id of space this file belongs to.
     */
    const std::string &getSpaceId(const folly::fbstring &uuid);

    /**
     * Ensures that file attributes and location is present in the cache by
     * fetching them from the server if missing.
     * @param uuid Uuid of the file.
     */
    void ensureAttrAndLocationCached(const folly::fbstring &uuid);

    /**
     * Removes all of file's metadata from the cache.
     * @param uuid Uuid of the file.
     */
    void erase(const folly::fbstring &uuid);

    /**
     * Truncates blocks in cached file locations and modifies attributes to set
     * the new size.
     * @param uuid Uuid of the file.
     * @param newSize Size to truncate to.
     */
    void truncate(const folly::fbstring &uuid, const std::size_t newSize);

    /**
     * Update times cached in file's attributes.
     * @param uuid Uuid of the file.
     * @param updateTimes Object containing new times.
     */
    void updateTimes(const folly::fbstring &uuid,
        const messages::fuse::UpdateTimes &updateTimes);

    /**
     * Changes mode cached in file's attributes.
     * @param uuid Uuid of the file.
     * @param newMode The new mode.
     */
    void changeMode(const folly::fbstring &uuid, const mode_t newMode);

    /**
     * Updates file attributes, if cached.
     * @param newAttr Updated attributes.
     * @returns true if attributes have been updated, false if they were not
     * cached.
     */
    bool updateAttr(const FileAttr &newAttr);

    /**
     * Updates file location, if cached.
     * @param newLocation Updated location.
     * @returns true if location has been updated, false if it was not cached.
     */
    bool updateLocation(const FileLocation &newLocation);

    /**
     * Marks file as deleted, preventing it from being looked up by parent.
     * @param uuid Uuid of the file.
     * @returns true if file has been marked as deleted, false if it was not
     * cached.
     */
    bool markDeleted(const folly::fbstring &uuid);

    /**
     * Renames a cached file.
     * @param uuid Curent uuid of the file.
     * @param newParentUuid Uuid of the file's new parent.
     * @param newName New name of the file.
     * @param newUuid New uuid of the file.
     * @returns true if file has been renamed, false if it was not cached.
     */
    bool rename(const folly::fbstring &uuid,
        const folly::fbstring &newParentUuid, const folly::fbstring &newName,
        const folly::fbstring &newUuid);

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

private:
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

    using Map = boost::multi_index::multi_index_container<Metadata,
        boost::multi_index::indexed_by<
            boost::multi_index::hashed_unique<boost::multi_index::tag<ByUuid>,
                UuidExtractor, std::hash<folly::fbstring>>,
            boost::multi_index::hashed_unique<
                boost::multi_index::tag<ByParent>,
                boost::multi_index::composite_key<Metadata, ParentUuidExtractor,
                    NameExtractor>,
                boost::multi_index::composite_key_hash<
                    std::hash<folly::fbstring>, std::hash<folly::fbstring>>>>>;

    Map::iterator getAttrIt(const folly::fbstring &uuid);

    template <typename ReqMsg> Map::iterator fetchAttr(ReqMsg &&msg);

    std::shared_ptr<FileLocation> getLocationPtr(const Map::iterator &it);

    std::shared_ptr<FileLocation> fetchFileLocation(
        const folly::fbstring &uuid);

    communication::Communicator &m_communicator;

    Map m_cache;

    std::function<void(const folly::fbstring &)> m_onMarkDeleted = [](auto) {};
    std::function<void(const folly::fbstring &, const folly::fbstring &)>
        m_onRename = [](auto, auto) {};
};

} // namespace cache
} // namespace client
} // namespace one

#endif // ONECLIENT_METADATA_CACHE_H
