/**
 * @file inodeCache.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include <boost/multi_index/hashed_index.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index_container.hpp>
#include <folly/FBString.h>
#include <folly/Optional.h>
#include <fuse/fuse_lowlevel.h>

namespace one {
namespace client {
namespace cache {

/**
 * @c InodeCache is responsible for translating between uuids and inodes.
 */
class InodeCache {
public:
    /**
     * Constructor.
     * @param rootUuid Uuid of the user's root directory that will be bound to
     * @c FUSE_ROOT_ID .
     * @param targetCacheSize The target size of the cache; the cache will
     * attempt to keep population no bigger than this number.
     */
    InodeCache(
        folly::fbstring rootUuid, const std::size_t targetCacheSize = 100000);

    /**
     * Looks up an number by its uuid and increments lookup count for the
     * entry.
     * A new inode is returned and cached if there's no known association.
     * @param uuid Uuid to look up by.
     * @returns Inode associated with the uuid.
     */
    fuse_ino_t lookup(const folly::fbstring &uuid);

    /**
     * Returns an uuid associated with the inode.
     * Throws an instance of @c std::out_of_range if inode is unknown.
     * @param ino Inode to look up by.
     * @returns Uuid associated with the inode.
     */
    folly::fbstring at(const fuse_ino_t ino) const;

    /**
     * Decrements lookup cound of a cached inode.
     * @param inode The cached inode.
     * @param count Number to decrement by.
     */
    void forget(const fuse_ino_t inode, const std::size_t count);

    /**
     * Renames an uuid in the cache.
     * @param oldUuid Uuid to rename from.
     * @param newUuid Uuid to rename to.
     */
    void rename(const folly::fbstring &oldUuid, folly::fbstring newUuid);

    /**
     * Marks uuid as deleted.
     * The entry will be removed from cache as soon as its lookup count reaches
     * 0.
     * @param uuid The uuid to mark as deleted.
     */
    void markDeleted(folly::fbstring uuid);

private:
    void prune();

    struct ByInode {
    };
    struct ByUuid {
    };

    struct Entry {
        Entry(fuse_ino_t, folly::fbstring);

        fuse_ino_t inode;
        folly::fbstring uuid;
        std::size_t lookupCount{1};
        folly::Optional<std::list<fuse_ino_t>::iterator> lruIt;
        bool deleted{false};
    };

    using Map = boost::multi_index::multi_index_container<Entry,
        boost::multi_index::indexed_by<
            boost::multi_index::ordered_unique<boost::multi_index::tag<ByInode>,
                boost::multi_index::member<Entry, fuse_ino_t, &Entry::inode>>,
            boost::multi_index::ordered_unique<boost::multi_index::tag<ByUuid>,
                boost::multi_index::member<Entry, folly::fbstring,
                    &Entry::uuid>>>>;

    const std::size_t m_targetCacheSize;
    Map m_cache;
    std::list<fuse_ino_t> m_lru;
    std::size_t m_nextInode = FUSE_ROOT_ID + 1;
};

} // namespace cache
} // namespace client
} // namespace one
