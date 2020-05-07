/**
 * @file openFileMetadataCache.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "metadataCache.h"

#include "communication/communicator.h"

#include <folly/FBString.h>
#include <folly/Optional.h>

#include <cstdint>
#include <list>
#include <memory>
#include <unordered_map>

namespace one {
namespace client {
namespace cache {

class ReaddirCache;

/**
 * @c OpenFileMetadataCache is responsible for ensuring that open files and
 * directories entries are cached in the @c MetadataCache .
 */
class OpenFileMetadataCache : private MetadataCache {
public:
    /**
     * @c OpenFileToken is a RAII wrapper for an open file.
     */
    class OpenFileToken {
    public:
        /**
         * Construtor.
         * @param attr File attributes, used to fetch current uuid on release.
         * @param cache An instance of cache on which the release will be
         * called.
         */
        OpenFileToken(FileAttrPtr attr, OpenFileMetadataCache &cache);

        /**
         * Destructor.
         * Calls release on the cache with @c attr->uuid() .
         */
        ~OpenFileToken();

        OpenFileToken(const OpenFileToken &) = delete;
        OpenFileToken(OpenFileToken &&) = delete;

    private:
        FileAttrPtr m_attr;
        OpenFileMetadataCache &m_cache;
    };

    /**
     * Constructor.
     * @param communicator @c Communicator instance that will be passed to @c
     * MetadataCache constructor.
     * @param targetSize The target size of the cache; the cache will attempt
     * to keep population no bigger than this number.
     */
    OpenFileMetadataCache(communication::Communicator &communicator,
        const std::size_t targetSize,
        const std::chrono::seconds providerTimeout,
        const std::chrono::seconds directoryCacheDropAfter,
        const folly::fbstring &rootUuid,
        const std::vector<std::string> &spaceNames,
        const std::vector<std::string> &spaceIds);

    /**
     * Sets a pointer to an instance of @c ReaddirCache.
     * @param readdirCache Shared pointer to an instance of @c ReaddirCache.
     */
    void setReaddirCache(std::shared_ptr<ReaddirCache> readdirCache);

    /**
     * Increases the open counter on the directory cache entry.
     * The directory is opened typically by Fuse opendir call, which is
     * released after directory contents have been listed.
     *
     * @param uuid UUID of the directory
     */
    void opendir(const folly::fbstring &uuid);

    /**
     * Decrements the open counter on the directory cache entry.
     *
     * @param uuid UUID of the directory
     */
    void releasedir(const folly::fbstring &uuid);

    /**
     * Returns the directory contents at specific offset and amount
     * from the metadata cache.
     *
     * @param uuid UUID of the directory
     * @param off Offset in the directory
     * @param chunkSize Number of entries which should be returned
     * @return List of file or directory names.
     */
    folly::fbvector<folly::fbstring> readdir(
        const folly::fbstring &uuid, off_t off, std::size_t chunkSize);

    /**
     * Opens a file in the cache.
     * @param uuid Uuid of the file.
     * @returns A token representing the open file.
     */
    std::shared_ptr<OpenFileToken> open(const folly::fbstring &uuid);

    /**
     * Opens a file in the cache and puts its attributes.
     * @param uuid Uuid of the file.
     * @param attr The file attributes to put in the cache.
     * @param attr The file location to put in the cache.
     * @returns A token representing the open file.
     */
    std::shared_ptr<OpenFileToken> open(const folly::fbstring &uuid,
        std::shared_ptr<FileAttr> attr, std::unique_ptr<FileLocation> location);

    /**
     * @copydoc MetadataCache::getattr(const folly::fbstring &)
     */
    FileAttrPtr getAttr(const folly::fbstring &uuid);

    /**
     * @copydoc MetadataCache::getattr(const folly::fbstring &, const
     * folly::fbstring &)
     */
    FileAttrPtr getAttr(
        const folly::fbstring &parentUuid, const folly::fbstring &name);

    /**
     * @copydoc MetadataCache::putattr(std::shared_ptr<FileAttr> attr)
     */
    void putAttr(std::shared_ptr<FileAttr> attr);

    /**
     * Sets a callback that will be called after a file is added to the cache.
     * @param cb The callback which takes uuid as parameter.
     */
    void onAdd(std::function<void(const folly::fbstring &)> cb)
    {
        m_onSyncDirectory = cb;
        MetadataCache::onAdd(std::move(cb));
    }

    /**
     * Sets a callback that will be called after a file is opened.
     * @param cb The callback which takes uuid as parameter.
     */
    void onOpen(std::function<void(const folly::fbstring &)> cb)
    {
        m_onOpen = std::move(cb);
    }

    /**
     * Sets a callback that will be called after a file is released (closed).
     * @param cb The callback which takes uuid as parameter.
     */
    void onRelease(std::function<void(const folly::fbstring &)> cb)
    {
        m_onRelease = std::move(cb);
    }

    /**
     * Sets a callback that will be called after a file is pruned from the
     * cache.
     * @param cb The callback which takes uuid as parameter.
     */
    void onDropFile(std::function<void(const folly::fbstring &)> cb)
    {
        m_onDropFile = std::move(cb);
    }

    /**
     * Sets a callback that will be called after a directory is pruned from the
     * cache.
     * @param cb The callback which takes uuid as parameter.
     */
    void onDropDirectory(std::function<void(const folly::fbstring &)> cb)
    {
        m_onDropDirectory = std::move(cb);
    }

    /**
     * @copydoc MetadataCache::onMarkDeleted(std::function<void(const
     * folly::fbstring &)>)
     */
    void onMarkDeleted(std::function<void(const folly::fbstring &)> cb)
    {
        m_onMarkDeleted = std::move(cb);
    }

    /**
     * @copydoc MetadataCache::onRename(std::function<void(const folly::fbstring
     * &, const folly::fbstring &)>)
     */
    void onRename(
        std::function<void(const folly::fbstring &, const folly::fbstring &)>
            cb)
    {
        m_onRename = std::move(cb);
    }

    /**
     * @copydoc MetadataCache::rename(const folly::fbstring &, const
     * folly::fbstring &, const folly::fbstring &, const folly::fbstring &)
     */
    bool rename(folly::fbstring uuid, folly::fbstring newParentUuid,
        folly::fbstring newName, folly::fbstring newUuid);

    /**
     * @copydoc MetadataCache::truncate(const folly::fbstring &, const
     * std::size_t)
     */
    void truncate(folly::fbstring uuid, const std::size_t newSize);

    /**
     * @copydoc MetadataCache::updateTimes(const folly::fbstring &, const
     * messages::fuse::UpdateTimes &)
     */
    void updateTimes(
        folly::fbstring uuid, const messages::fuse::UpdateTimes &updateTimes);

    /**
     * @copydoc MetadataCache::changeMode(const folly::fbstring &, const mode_t)
     */
    void changeMode(const folly::fbstring &uuid, const mode_t newMode);

    /**
     * @copydoc MetadataCache::putLocation(std::unique_ptr<FileLocation>);
     */
    void putLocation(std::unique_ptr<FileLocation> location);

    /**
     * @copydoc MetadataCache::getLocation(const folly::fbstring &uuid, bool
     * forceUpdate = false);
     */
    std::shared_ptr<FileLocation> getLocation(
        const folly::fbstring &uuid, bool forceUpdate = false);

    /**
     * @copydoc MetadataCache::updateLocation(const FileLocation &newLocation);
     */
    bool updateLocation(const FileLocation &newLocation);

    /**
     * @copydoc MetadataCache::updateLocation(const off_t start, const off_t
     * end, const FileLocation &locationUpdate);
     */
    bool updateLocation(
        const off_t start, const off_t end, const FileLocation &locationUpdate);

    /**
     * Removes from metadata cache directories which haven't been used recently.
     */
    void pruneExpiredDirectories();

    /**
     * Returns true if the directory uuid has been at least once synced from the
     * server through readdir.
     * @param uuid UUID of the synced directory.
     */
    bool isDirectorySynced(const folly::fbstring &uuid);

    /**
     * Marks a directory as synced, after readdir has been complete.
     * @param uuid UUID of the synced directory.
     */
    void setDirectorySynced(const folly::fbstring &uuid);

    /**
     * Adds a specific block to a cached file locations. File location must be
     * present in the cache.
     *
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
     *
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
     *
     * @param uuid Uuid of the file.
     * @returns File block.
     */
    messages::fuse::FileBlock getDefaultBlock(const folly::fbstring &uuid);

    /**
     * Retrieves space Id by uuid.
     *
     * @param uuid Uuid of the file.
     * @returns Id of space this file belongs to.
     */
    const std::string &getSpaceId(const folly::fbstring &uuid);

    /**
     * Updates file attributes, if cached.
     *
     * @param newAttr Updated attributes.
     * @returns true if attributes have been updated, false if they were not
     * cached.
     */
    bool updateAttr(std::shared_ptr<FileAttr> newAttr);

    using MetadataCache::contains;
    using MetadataCache::markDeleted;
    using MetadataCache::putAttr;
    using MetadataCache::size;

private:
    /**
     * This structure contains information about currently opened files along
     * with their open count or directories for which file attributes have been
     * accessed recently.
     */
    struct OpenFileData {
        OpenFileData()
            : lastUsed{std::chrono::system_clock::now()}
            , openCount{0}
            , deleted{false}
            , dirRead{false}
        {
        }

        // Shared pointer to the file attr, necessary for deleted opened files
        // or directories
        std::shared_ptr<FileAttr> attr;

        // Shared pointer to the file location, necessary for deleted opened
        // files
        std::shared_ptr<FileLocation> location;

        // When was the file or directory last used
        std::chrono::system_clock::time_point lastUsed;

        // Number of open file descriptors for given file, in case the entry is
        // directory, the openCount is a sum of all files opened in this
        // directory This is used to decide whether a directory subscription can
        // be cancelled
        std::size_t openCount;

        // True when file or directory has been marked as deleted, but is still
        // opened
        bool deleted;

        // True when this directory has been synced through readdir at least
        // once
        bool dirRead;

        // Iterator to current position in LRU list, to optimize search for
        // uuid in the LRU list
        folly::Optional<std::list<folly::fbstring>::iterator> lruIt;

        void touch() { lastUsed = std::chrono::system_clock::now(); }

        bool expired(const std::chrono::seconds &expireAfter)
        {
            using namespace std::literals::chrono_literals;

            if (expireAfter.count() == 0)
                return false;

            return (std::chrono::system_clock::now() - lastUsed) > expireAfter;
        }
    };

    /**
     * Mark file as opened, and add it to the m_lruList. In case the file
     * is opened, increase the open count.
     * @param uuid UUID of the opened file.
     * @param parentUuid UUID of the parent of the opened file.
     */
    void pinFile(const folly::fbstring &uuid);

    /**
     * Update the directory list to reflect an activity in a directory
     * `uuid`
     * @parma uuid UUID of the directory where some activity occured
     */
    void noteDirectoryActivity(const folly::fbstring &uuid);

    /**
     * Release a file from cache, when a file descriptor for this file
     * has been closed.
     * @param uuid UUID of the closed file
     */
    void releaseFile(const folly::fbstring &uuid);

    void prune();

    void onPrune();

    void handleMarkDeleted(const folly::fbstring &uuid);

    void handleRename(
        const folly::fbstring &oldUuid, const folly::fbstring &newUuid);

    const std::size_t m_targetSize;
    const std::chrono::seconds m_directoryCacheDropAfter;

    std::mutex m_lruMutex;
    std::list<folly::fbstring> m_lruFileList;
    std::unordered_map<folly::fbstring, OpenFileData> m_lruFileData;
    std::list<folly::fbstring> m_lruDirectoryList;
    std::unordered_map<folly::fbstring, OpenFileData> m_lruDirectoryData;

    std::function<void(const folly::fbstring &)> m_onSyncDirectory =
        [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onOpen = [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onRelease = [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onDropFile = [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onDropDirectory =
        [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onMarkDeleted = [](auto) {};
    std::function<void(const folly::fbstring &, const folly::fbstring &)>
        m_onRename = [](auto, auto) {};
};

} // namespace cache
} // namespace client
} // namespace one
