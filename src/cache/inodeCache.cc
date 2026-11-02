/**
 * @file inodeCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "inodeCache.h"
#include "helpers/logging.h"
#include "monitoring/monitoring.h"

#include <cassert>
#include <stdexcept>
#include <string>
#include <vector>

namespace one {
namespace client {
namespace cache {

InodeCache::Entry::Entry(const fuse_ino_t inode_, folly::fbstring uuid_,
    folly::fbstring providerId_, folly::fbstring key_,
    const fuse_ino_t virtualParent_)
    : inode{inode_}
    , uuid{std::move(uuid_)}
    , providerId{std::move(providerId_)}
    , key{std::move(key_)}
    , virtualParent{virtualParent_}
{
}

InodeCache::InodeCache(
    const folly::fbstring &rootUuid, const std::size_t targetCacheSize)
    : m_targetCacheSize{targetCacheSize}
{
    m_cache.emplace(FUSE_ROOT_ID, rootUuid, folly::fbstring{}, rootUuid);
    lookup(rootUuid);

    ONE_METRIC_COUNTER_SET(
        "comp.oneclient.mod.inodecache.maxsize", targetCacheSize);
}

folly::fbstring InodeCache::providerId(const fuse_ino_t inode) const
{
    LOG_FCALL() << LOG_FARG(inode);

    const auto &index = boost::multi_index::get<ByInode>(m_cache);
    auto entryIt = index.find(inode);
    if (entryIt == index.end() || entryIt->lruIt) {
        LOG(ERROR) << "No provider found for inode " << inode;
        throw std::out_of_range{
            "no active mapping for inode " + std::to_string(inode)};
    }

    LOG_DBG(2) << "Returning providerId " << entryIt->uuid << " for inode "
               << inode;

    return entryIt->providerId;
}

fuse_ino_t InodeCache::generateInode(
    const folly::fbstring &uuid, const folly::fbstring &providerId)
{
    auto &index = boost::multi_index::get<ByKey>(m_cache);
    auto entryIt = index.find(uuid);

    if (entryIt == index.end()) {
        const auto inode = m_nextInode++;
        m_cache.emplace(inode, uuid, providerId, uuid);

        LOG_DBG(2) << "Created new inode " << inode << " for file " << uuid;

        prune();
    }

    return lookup(uuid);
}

fuse_ino_t InodeCache::generateVirtualInode(const folly::fbstring &uuid,
    const folly::fbstring &providerId, const fuse_ino_t parent)
{
    assert(parent != 0);
    const folly::fbstring key =
        uuid.toStdString() + ":" + std::to_string(parent);
    auto &index = boost::multi_index::get<ByKey>(m_cache);
    if (index.find(key) == index.end()) {
        m_cache.emplace(m_nextInode++, uuid, providerId, key, parent);
        prune();
    }
    return lookupKey(key);
}

bool InodeCache::isVirtualInode(const fuse_ino_t inode) const
{
    const auto &index = boost::multi_index::get<ByInode>(m_cache);
    auto it = index.find(inode);
    return it != index.end() && it->virtualParent != 0;
}

fuse_ino_t InodeCache::lookup(const folly::fbstring &uuid)
{
    return lookupKey(uuid);
}

fuse_ino_t InodeCache::lookupKey(const folly::fbstring &key)
{
    LOG_FCALL() << LOG_FARG(key);

    auto &index = boost::multi_index::get<ByKey>(m_cache);
    auto entryIt = index.find(key);

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

        LOG_DBG(2) << "Found inode " << entryIt->inode << " for key " << key;

        return entryIt->inode;
    }

    LOG(ERROR) << "No inode found for key " << key;
    throw std::out_of_range{"no active mapping for key " + key.toStdString()};
}

std::pair<folly::fbstring, folly::fbstring> InodeCache::at(
    const fuse_ino_t inode) const
{
    LOG_FCALL() << LOG_FARG(inode);

    const auto &index = boost::multi_index::get<ByInode>(m_cache);
    auto entryIt = index.find(inode);
    if (entryIt == index.end() || entryIt->lruIt) {
        LOG(ERROR) << "No file found for inode " << inode;
        throw std::out_of_range{
            "no active mapping for inode " + std::to_string(inode)};
    }

    LOG_DBG(2) << "Returning file " << entryIt->uuid << " for inode " << inode;

    return {entryIt->uuid, entryIt->providerId};
}

void InodeCache::forget(const fuse_ino_t inode, const std::size_t count)
{
    LOG_FCALL() << LOG_FARG(inode) << LOG_FARG(count);

    auto &index = boost::multi_index::get<ByInode>(m_cache);
    auto entryIt = index.find(inode);

    if (entryIt == index.end()) {
        LOG_DBG(2) << "Inode " << inode
                   << " not found in inode cache - ignoring...";
        return;
    }

    assert(entryIt->lookupCount >= count);

    const auto newCount = entryIt->lookupCount - count;

    if (newCount > 0) {
        LOG_DBG(2) << "Changing inode " << inode << " lookup count to "
                   << newCount;
        index.modify(entryIt, [&](Entry &e) { e.lookupCount = newCount; });
    }
    else if (entryIt->deleted || entryIt->virtualParent != 0) {
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

void InodeCache::rename(const folly::fbstring &oldUuid, folly::fbstring newUuid,
    const folly::fbstring & /*newParentUuid*/)
{
    LOG_FCALL() << LOG_FARG(oldUuid) << LOG_FARG(newUuid);

    // Snapshot node IDs: modifying UUIDs changes the ByUuid index ordering.
    auto &byUuid = boost::multi_index::get<ByUuid>(m_cache);
    const auto range = byUuid.equal_range(oldUuid);
    std::vector<fuse_ino_t> inodes;
    for (auto it = range.first; it != range.second; ++it)
        inodes.push_back(it->inode);

    auto &byInode = boost::multi_index::get<ByInode>(m_cache);
    auto &byKey = boost::multi_index::get<ByKey>(m_cache);
    for (auto inode : inodes) {
        auto it = byInode.find(inode);
        auto target = byKey.find(newUuid);
        const bool targetExists =
            target != byKey.end() && target->inode != inode;
        byInode.modify(it, [&](Entry &entry) {
            entry.uuid = newUuid;
            if (entry.virtualParent != 0 || targetExists) {
                // The kernel may have moved this node to the rename target.
                // Keep it usable by existing handles, but allocate a new node
                // on the next lookup through its original virtual parent.
                entry.key = "retired:" + std::to_string(entry.inode);
            }
            else if (entry.key == oldUuid)
                entry.key = newUuid;
        });
    }
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
