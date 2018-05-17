/**
 * @file inodeCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "inodeCache.h"
#include "logging.h"
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
    LOG_FCALL() << LOG_FARG(uuid);

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto entryIt = index.find(uuid);

    if (entryIt != index.end()) {
        if (entryIt->lruIt) {
            m_lru.erase(*entryIt->lruIt);
            index.modify(entryIt, [](Entry &entry) {
                entry.lruIt.clear();
                ++entry.lookupCount;
            });
        }
        else
            index.modify(entryIt, [](Entry &e) { ++e.lookupCount; });

        LOG_DBG(2) << "Found inode " << entryIt->inode << " for file " << uuid;

        return entryIt->inode;
    }

    const auto inode = m_nextInode++;
    m_cache.emplace(inode, std::move(uuid));

    LOG_DBG(2) << "Created new inode " << inode << " for file " << uuid;

    prune();

    ONE_METRIC_COUNTER_SET("comp.oneclient.mod.inodecache.size", index.size());

    return inode;
}

folly::fbstring InodeCache::at(const fuse_ino_t inode) const
{
    LOG_FCALL() << LOG_FARG(inode);

    auto &index = boost::multi_index::get<ByInode>(m_cache);
    auto entryIt = index.find(inode);
    if (entryIt == index.end() || entryIt->lruIt) {
        LOG(ERROR) << "No file found for inode " << inode;
        throw std::out_of_range{
            "no active mapping for inode " + std::to_string(inode)};
    }

    LOG_DBG(2) << "Returning file " << entryIt->uuid << " for inode " << inode;

    return entryIt->uuid;
}

void InodeCache::forget(const fuse_ino_t inode, const std::size_t count)
{
    LOG_FCALL() << LOG_FARG(inode) << LOG_FARG(count);

    auto &index = boost::multi_index::get<ByInode>(m_cache);
    auto entryIt = index.find(inode);

    assert(entryIt != index.end());
    assert(entryIt->lookupCount >= count);

    const auto newCount = entryIt->lookupCount - count;

    if (newCount > 0) {
        LOG_DBG(2) << "Changing inode " << inode << " lookup count to "
                   << newCount;
        index.modify(entryIt, [&](Entry &e) { e.lookupCount = newCount; });
    }
    else if (entryIt->deleted) {
        LOG_DBG(2) << "Removing deleted inode " << inode << " from inode cache";
        index.erase(entryIt);
    }
    else {
        LOG_DBG(2) << "Modifying entry lookup count to " << newCount
                   << " and lru index";
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
    LOG_FCALL() << LOG_FARG(oldUuid) << LOG_FARG(newUuid);

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    const auto entryRange = index.equal_range(oldUuid);
    for (auto it = entryRange.first; it != entryRange.second; ++it)
        index.modify_key(it, [&](folly::fbstring &key) { key.swap(newUuid); });
}

void InodeCache::markDeleted(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    const auto entryRange = index.equal_range(uuid);
    for (auto it = entryRange.first; it != entryRange.second; ++it)
        index.modify(it, [](Entry &e) { e.deleted = true; });
}

void InodeCache::prune()
{
    LOG_FCALL();

    auto &index = boost::multi_index::get<ByInode>(m_cache);
    if (m_cache.size() > m_targetCacheSize && !m_lru.empty()) {
        index.erase(m_lru.front());
        m_lru.pop_front();
    }
}

} // namespace cache
} // namespace client
} // namespace one
