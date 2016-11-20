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
        const std::size_t targetSize = 1000);

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
     * @returns A token representing the open file.
     */
    std::shared_ptr<OpenFileToken> open(
        const folly::fbstring &uuid, std::shared_ptr<FileAttr> attr);

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
     * Sets a callback that will be called after a file is added to the cache.
     * @param cb The callback which takes uuid as parameter.
     */
    void onAdd(std::function<void(const folly::fbstring &)> cb)
    {
        m_onAdd = std::move(cb);
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
     * Sets a callback that will be called after a file is pruned from the
     * cache.
     * @param cb The callback which takes uuid as parameter.
     */
    void onPrune(std::function<void(const folly::fbstring &)> cb)
    {
        m_onPrune = std::move(cb);
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
    bool rename(const folly::fbstring &uuid,
        const folly::fbstring &newParentUuid, const folly::fbstring &newName,
        const folly::fbstring &newUuid);

    /**
     * @copydoc MetadataCache::truncate(const folly::fbstring &, const
     * std::size_t)
     */
    void truncate(const folly::fbstring &uuid, const std::size_t newSize);

    /**
     * @copydoc MetadataCache::updateTimes(const folly::fbstring &, const
     * messages::fuse::UpdateTimes &)
     */
    void updateTimes(const folly::fbstring &uuid,
        const messages::fuse::UpdateTimes &updateTimes);

    /**
     * @copydoc MetadataCache::changeMode(const folly::fbstring &, const mode_t)
     */
    void changeMode(const folly::fbstring &uuid, const mode_t newMode);

    /**
     * @copydoc MetadataCache::putLocation(std::unique_ptr<FileLocation>);
     */
    void putLocation(std::unique_ptr<FileLocation> location);

    // Operations used only on open files
    using MetadataCache::addBlock;
    using MetadataCache::getFileLocation;
    using MetadataCache::getBlock;

    using MetadataCache::updateFileAttr;
    using MetadataCache::putAttr;
    using MetadataCache::updateFileLocation;
    using MetadataCache::markDeleted;

private:
    struct LRUData {
        std::size_t openCount = 0;
        bool deleted = false;
        folly::Optional<std::list<folly::fbstring>::iterator> lruIt;
    };

    void noteActivity(const folly::fbstring &uuid);

    void release(const folly::fbstring &uuid);

    void prune();

    void handleMarkDeleted(const folly::fbstring &uuid);

    void handleRename(
        const folly::fbstring &oldUuid, const folly::fbstring &newUuid);

    const std::size_t m_targetSize;

    std::list<folly::fbstring> m_lruList;
    std::unordered_map<folly::fbstring, LRUData> m_lruData;

    std::function<void(const folly::fbstring &)> m_onAdd = [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onOpen = [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onPrune = [](auto &) {};
    std::function<void(const folly::fbstring &)> m_onMarkDeleted = [](auto) {};
    std::function<void(const folly::fbstring &, const folly::fbstring &)>
        m_onRename = [](auto, auto) {};
};

} // namespace cache
} // namespace client
} // namespace one
