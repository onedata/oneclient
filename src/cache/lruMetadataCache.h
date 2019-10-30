/**
 * @file lruMetadataCache.h
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
 * @c LRUMetadataCache is responsible for managing lifetime of entries cached
 * in @c MetadataCache .
 */
class LRUMetadataCache : private MetadataCache {
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
        OpenFileToken(FileAttrPtr attr, LRUMetadataCache &cache);

        /**
         * Destructor.
         * Calls release on the cache with @c attr->uuid() .
         */
        ~OpenFileToken();

        OpenFileToken(const OpenFileToken &) = delete;
        OpenFileToken(OpenFileToken &&) = delete;

    private:
        FileAttrPtr m_attr;
        LRUMetadataCache &m_cache;
    };

    /**
     * Constructor.
     * @param communicator @c Communicator instance that will be passed to @c
     * MetadataCache constructor.
     * @param targetSize The target size of the cache; the cache will attempt
     * to keep population no bigger than this number.
     */
    LRUMetadataCache(communication::Communicator &communicator,
        const std::size_t targetSize,
        const std::chrono::seconds providerTimeout);

    /**
     * Sets a pointer to an instance of @c ReaddirCache.
     * @param readdirCache Shared pointer to an instance of @c ReaddirCache.
     */
    void setReaddirCache(std::shared_ptr<ReaddirCache> readdirCache);

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

    // Operations used only on open files
    using MetadataCache::addBlock;
    using MetadataCache::getBlock;
    using MetadataCache::getDefaultBlock;
    using MetadataCache::getSpaceId;

    using MetadataCache::markDeleted;
    using MetadataCache::putAttr;
    using MetadataCache::size;
    using MetadataCache::updateAttr;

private:
    /**
     * This structure contains information about currently opened files along
     * with their open count or directories for which file attributes have been
     * accessed recently.
     */
    struct LRUData {
        LRUData()
            : lastUsed{std::chrono::system_clock::now()}
            , openCount{0}
            , deleted{false}
        {
        }

        // When was the file or directory last used
        std::chrono::system_clock::time_point lastUsed;

        // Number of open file descriptors for given file, in case the entry is
        // directory, the openCount is a sum of all files opened in this
        // directory This is used to decide whether a directory subscription can
        // be cancelled
        std::size_t openCount;

        // True when file has been marked as deleted, but is still opened
        bool deleted;

        // Iterator to current position in the lru list, to optimize search for
        // uuid in the LRU list
        folly::Optional<std::list<folly::fbstring>::iterator> lruIt;

        void touch() { lastUsed = std::chrono::system_clock::now(); }

        bool expired()
        {
            using namespace std::literals::chrono_literals;
            return (std::chrono::system_clock::now() - lastUsed) > 30s;
        }
    };

    /**
     * Mark file as opened, and add it to the m_lruList. In case the file
     * is opened, increase the open count.
     * @param uuid UUID of the opened file.
     * @param parentUuid UUID of the parent of the opened file.
     */
    void pinFile(const folly::fbstring &uuid);
    void pinDirectory(const folly::fbstring &uuid);

    /**
     * Update the LRU directory list to reflect an activity in a directory
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
    void releaseDirectory(const folly::fbstring &uuid);

    void prune();

    void onPrune();

    void handleMarkDeleted(const folly::fbstring &uuid);

    void handleRename(
        const folly::fbstring &oldUuid, const folly::fbstring &newUuid);

    const std::size_t m_targetSize;

    std::list<folly::fbstring> m_lruFileList;
    std::unordered_map<folly::fbstring, LRUData> m_lruFileData;
    std::list<folly::fbstring> m_lruDirectoryList;
    std::unordered_map<folly::fbstring, LRUData> m_lruDirectoryData;

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
