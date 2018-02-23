#include "lruMetadataCache.h"

#include "cache/readdirCache.h"
#include "logging.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"

#include <functional>

namespace one {
namespace client {
namespace cache {

LRUMetadataCache::OpenFileToken::OpenFileToken(
    FileAttrPtr attr, LRUMetadataCache &cache)
    : m_attr{std::move(attr)}
    , m_cache{cache}
{
}

LRUMetadataCache::OpenFileToken::~OpenFileToken()
{
    m_cache.release(m_attr->uuid());
}

LRUMetadataCache::LRUMetadataCache(communication::Communicator &communicator,
    const std::size_t targetSize, const std::chrono::seconds providerTimeout)
    : MetadataCache{communicator, providerTimeout}
    , m_targetSize{targetSize}
{
    using namespace std::placeholders;

    MetadataCache::onRename(
        std::bind(&LRUMetadataCache::handleRename, this, _1, _2));

    MetadataCache::onMarkDeleted(
        std::bind(&LRUMetadataCache::handleMarkDeleted, this, _1));
}

void LRUMetadataCache::setReaddirCache(
    std::shared_ptr<ReaddirCache> readdirCache)
{
    MetadataCache::setReaddirCache(readdirCache);
}

void LRUMetadataCache::pinEntry(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto res = m_lruData.emplace(uuid, LRUData{});
    auto &lruData = res.first->second;

    ++lruData.openCount;

    LOG_DBG(2) << "Increased LRU open count of " << uuid << " to "
               << lruData.openCount;

    if (lruData.lruIt) {
        m_lruList.erase(*lruData.lruIt);
        lruData.lruIt.clear();

        m_onOpen(uuid);
    }
}

std::shared_ptr<LRUMetadataCache::OpenFileToken> LRUMetadataCache::open(
    const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    pinEntry(uuid);

    try {
        auto attr = MetadataCache::getAttr(uuid);
        MetadataCache::ensureAttrAndLocationCached(uuid);
        prune();
        return std::make_shared<OpenFileToken>(std::move(attr), *this);
    }
    catch (...) {
        LOG(ERROR) << " Removing " << uuid
                   << " from LRU metadata cache due to unexpected error.";
        release(uuid);
        throw;
    }
}

std::shared_ptr<LRUMetadataCache::OpenFileToken> LRUMetadataCache::open(
    const folly::fbstring &uuid, std::shared_ptr<FileAttr> attr,
    std::unique_ptr<FileLocation> location)
{
    LOG_FCALL() << LOG_FARG(uuid);

    pinEntry(uuid);

    MetadataCache::putAttr(attr);
    MetadataCache::putLocation(std::move(location));
    prune();
    return std::make_shared<OpenFileToken>(std::move(attr), *this);
}

void LRUMetadataCache::release(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto it = m_lruData.find(uuid);
    if (it == m_lruData.end())
        return;

    if (--it->second.openCount)
        return;

    if (it->second.deleted) {
        m_lruData.erase(it);
        MetadataCache::erase(uuid);
    }
    else {
        it->second.lruIt = m_lruList.emplace(m_lruList.end(), uuid);
        prune();
    }
}

FileAttrPtr LRUMetadataCache::getAttr(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto attr = MetadataCache::getAttr(uuid);
    noteActivity(uuid);
    return attr;
}

FileAttrPtr LRUMetadataCache::getAttr(
    const folly::fbstring &parentUuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name);

    auto attr = MetadataCache::getAttr(parentUuid, name);
    noteActivity(attr->uuid());
    return attr;
}

void LRUMetadataCache::putAttr(std::shared_ptr<FileAttr> attr)
{
    LOG_FCALL();

    MetadataCache::putAttr(attr);
    noteActivity(attr->uuid());
}

void LRUMetadataCache::noteActivity(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto res = m_lruData.emplace(uuid, LRUData{});

    if (res.second) {
        res.first->second.lruIt = m_lruList.emplace(m_lruList.end(), uuid);
        m_onAdd(uuid);
    }
    else if (res.first->second.lruIt) {
        m_lruList.splice(m_lruList.end(), m_lruList, *res.first->second.lruIt);
    }

    prune();
}

void LRUMetadataCache::prune()
{
    LOG_FCALL();

    if (m_lruData.size() > m_targetSize && !m_lruList.empty()) {
        LOG_DBG(1) << "Pruning LRU metadata cache front because it exceeds "
                      "target size ("
                   << m_lruData.size() << ">" << m_targetSize << ")";
        auto uuid = std::move(m_lruList.front());
        m_lruList.pop_front();
        m_lruData.erase(uuid);
        MetadataCache::erase(uuid);
        m_onPrune(uuid);
    }
}

bool LRUMetadataCache::rename(const folly::fbstring &uuid,
    const folly::fbstring &newParentUuid, const folly::fbstring &newName,
    const folly::fbstring &newUuid)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newParentUuid)
                << LOG_FARG(newName) << LOG_FARG(newUuid);

    noteActivity(uuid);
    return MetadataCache::rename(uuid, newParentUuid, newName, newUuid);
}

void LRUMetadataCache::truncate(
    const folly::fbstring &uuid, const std::size_t newSize)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newSize);

    noteActivity(uuid);
    MetadataCache::truncate(uuid, newSize);
}

void LRUMetadataCache::updateTimes(
    const folly::fbstring &uuid, const messages::fuse::UpdateTimes &updateTimes)
{
    LOG_FCALL() << LOG_FARG(uuid);

    noteActivity(uuid);
    MetadataCache::updateTimes(uuid, updateTimes);
}

void LRUMetadataCache::changeMode(
    const folly::fbstring &uuid, const mode_t newMode)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newMode);

    noteActivity(uuid);
    MetadataCache::changeMode(uuid, newMode);
}

void LRUMetadataCache::putLocation(std::unique_ptr<FileLocation> location)
{
    LOG_FCALL();

    noteActivity(location->uuid());
    MetadataCache::putLocation(std::move(location));
}

void LRUMetadataCache::handleMarkDeleted(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto it = m_lruData.find(uuid);
    if (it == m_lruData.end())
        return;

    it->second.deleted = true;

    if (it->second.lruIt) {
        m_lruList.erase(*it->second.lruIt);
        m_lruData.erase(it);
        MetadataCache::erase(uuid);
    }

    m_onMarkDeleted(uuid);
}

void LRUMetadataCache::handleRename(
    const folly::fbstring &oldUuid, const folly::fbstring &newUuid)
{
    LOG_FCALL() << LOG_FARG(oldUuid) << LOG_FARG(newUuid);

    auto it = m_lruData.find(oldUuid);
    if (it == m_lruData.end())
        return;

    auto lruData = std::move(it->second);
    m_lruData.erase(it);

    auto res = m_lruData.emplace(newUuid, LRUData{});
    if (res.second) {
        res.first->second = std::move(lruData);
        if (lruData.lruIt) {
            auto oldIt = *lruData.lruIt;
            lruData.lruIt = m_lruList.emplace(oldIt, newUuid);
            m_lruList.erase(oldIt);
        }
    }
    else {
        LOG(WARNING) << "Target UUID '" << newUuid
                     << "' of rename is already used; merging metadata "
                        "usage records.";

        auto &oldRecord = res.first->second;
        oldRecord.openCount += lruData.openCount;
        oldRecord.deleted = oldRecord.deleted || lruData.deleted;

        if (lruData.lruIt)
            m_lruList.erase(*lruData.lruIt);
    }

    m_onRename(oldUuid, newUuid);
}

} // namespace cache
} // namespace client
} // namespace one
