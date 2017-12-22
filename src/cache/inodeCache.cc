/**
 * @file inodeCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "inodeCache.h"
#include "monitoring/monitoring.h"

#include <cassert>
#include <stdexcept>
#include <string>

namespace one {
namespace client {
namespace cache {

InodeCache::Entry::Entry(const fuse_ino_t inode_, folly::fbstring uuid_)
    : inode{inode_}
    , uuid{std::move(uuid_)}
{
}

InodeCache::InodeCache(
    folly::fbstring rootUuid, const std::size_t targetCacheSize)
    : m_targetCacheSize{targetCacheSize}
{
    m_cache.emplace(FUSE_ROOT_ID, rootUuid);
    ONE_METRIC_COUNTER_SET(
        "comp.oneclient.mod.inodecache.maxsize", targetCacheSize);
}

fuse_ino_t InodeCache::lookup(const folly::fbstring &uuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto entryIt = index.find(uuid);

    if (entryIt != index.end()) {
        if (entryIt->lruIt) {
            m_lru.erase(*entryIt->lruIt);
            index.modify(entryIt, [this](Entry &entry) {
                entry.lruIt.clear();
                ++entry.lookupCount;
            });
        }
        else
            index.modify(entryIt, [](Entry &e) { ++e.lookupCount; });

        return entryIt->inode;
    }

    const auto inode = m_nextInode++;
    m_cache.emplace(inode, std::move(uuid));

    prune();

    ONE_METRIC_COUNTER_SET("comp.oneclient.mod.inodecache.size", index.size());

    return inode;
}

folly::fbstring InodeCache::at(const fuse_ino_t inode) const
{
    auto &index = boost::multi_index::get<ByInode>(m_cache);
    auto entryIt = index.find(inode);
    if (entryIt == index.end() || entryIt->lruIt)
        throw std::out_of_range{
            "no active mapping for inode " + std::to_string(inode)};

    return entryIt->uuid;
}

void InodeCache::forget(const fuse_ino_t inode, const std::size_t count)
{
    auto &index = boost::multi_index::get<ByInode>(m_cache);
    auto entryIt = index.find(inode);

    assert(entryIt != index.end());
    assert(entryIt->lookupCount >= count);

    const auto newCount = entryIt->lookupCount - count;

    if (newCount > 0) {
        index.modify(entryIt, [&](Entry &e) { e.lookupCount = newCount; });
    }
    else if (entryIt->deleted) {
        index.erase(entryIt);
    }
    else {
        const auto newLruIt = m_lru.insert(m_lru.end(), inode);
        index.modify(entryIt, [&](Entry &entry) {
            entry.lookupCount = newCount;
            entry.lruIt = newLruIt;
        });

        prune();
    }
    ONE_METRIC_COUNTER_SET("comp.oneclient.mod.inodecache.size", index.size());
}

void InodeCache::rename(const folly::fbstring &oldUuid, folly::fbstring newUuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    const auto entryRange = index.equal_range(oldUuid);
    for (auto it = entryRange.first; it != entryRange.second; ++it)
        index.modify_key(it, [&](folly::fbstring &key) { key.swap(newUuid); });
}

void InodeCache::markDeleted(const folly::fbstring &uuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    const auto entryRange = index.equal_range(uuid);
    for (auto it = entryRange.first; it != entryRange.second; ++it)
        index.modify(it, [](Entry &e) { e.deleted = true; });
}

void InodeCache::prune()
{
    auto &index = boost::multi_index::get<ByInode>(m_cache);
    if (m_cache.size() > m_targetCacheSize && !m_lru.empty()) {
        index.erase(m_lru.front());
        m_lru.pop_front();
    }
}

} // namespace cache
} // namespace client
} // namespace one
