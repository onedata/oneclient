#include "lruMetadataCache.h"

#include "cache/readdirCache.h"
#include "helpers/logging.h"
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
    // OpenFileTokens are only created for files, never directories
    m_cache.releaseFile(m_attr->uuid());
}

LRUMetadataCache::LRUMetadataCache(communication::Communicator &communicator,
    const std::size_t targetSize, const std::chrono::seconds providerTimeout)
    : MetadataCache{communicator, providerTimeout}
    , m_targetSize{targetSize}
{
    MetadataCache::onRename(std::bind(&LRUMetadataCache::handleRename, this,
        std::placeholders::_1, std::placeholders::_2));

    MetadataCache::onMarkDeleted(std::bind(
        &LRUMetadataCache::handleMarkDeleted, this, std::placeholders::_1));
}

void LRUMetadataCache::setReaddirCache(
    std::shared_ptr<ReaddirCache> readdirCache)
{
    MetadataCache::setReaddirCache(readdirCache);
}

void LRUMetadataCache::pinFile(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto res = m_lruFileData.emplace(uuid, LRUData{});

    auto &lruData = res.first->second;

    ++lruData.openCount;

    LOG_DBG(2) << "Increased LRU open count of " << uuid << " to "
               << lruData.openCount;

    if (lruData.lruIt) {
        m_lruFileList.erase(*lruData.lruIt);
        lruData.lruIt.clear();

        m_onOpen(uuid);
    }
}

void LRUMetadataCache::pinDirectory(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto res = m_lruDirectoryData.emplace(uuid, LRUData{});

    auto &lruData = res.first->second;

    ++lruData.openCount;

    LOG(ERROR) << "Increased LRU directory children open count of " << uuid
               << " to " << lruData.openCount;

    if (lruData.lruIt) {
        m_lruDirectoryList.erase(*lruData.lruIt);
        lruData.lruIt.clear();
    }
}

std::shared_ptr<LRUMetadataCache::OpenFileToken> LRUMetadataCache::open(
    const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    try {
        auto attr = MetadataCache::getAttr(uuid);
        MetadataCache::ensureAttrAndLocationCached(uuid);

        pinFile(uuid);
        if (attr->parentUuid())
            pinDirectory(*attr->parentUuid());

        prune();

        return std::make_shared<OpenFileToken>(std::move(attr), *this);
    }
    catch (...) {
        LOG(ERROR) << " Removing " << uuid
                   << " from LRU metadata cache due to unexpected error.";
        releaseFile(uuid);
        throw;
    }
}

std::shared_ptr<LRUMetadataCache::OpenFileToken> LRUMetadataCache::open(
    const folly::fbstring &uuid, std::shared_ptr<FileAttr> attr,
    std::unique_ptr<FileLocation> location)
{
    LOG_FCALL() << LOG_FARG(uuid);

    pinFile(uuid);
    if (attr->parentUuid())
        pinDirectory(*attr->parentUuid());

    MetadataCache::putAttr(attr);
    MetadataCache::putLocation(std::move(location));
    prune();
    return std::make_shared<OpenFileToken>(std::move(attr), *this);
}

void LRUMetadataCache::releaseFile(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto it = m_lruFileData.find(uuid);
    if (it == m_lruFileData.end())
        return;

    // If there are other file handles referring to this file
    // do nothing
    if (--it->second.openCount > 0)
        return;

    // Call on release handlers
    m_onRelease(uuid);

    // If the file has been marked as deleted before it was closed,
    // remove it now from the cache, otherwise put it at the end of
    // lru list and call prune
    if (it->second.deleted) {
        m_lruFileData.erase(it);
        MetadataCache::erase(uuid);
    }
    else {
        it->second.lruIt = m_lruFileList.emplace(m_lruFileList.end(), uuid);
        prune();
    }
}

void LRUMetadataCache::releaseDirectory(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto it = m_lruDirectoryData.find(uuid);
    if (it == m_lruDirectoryData.end())
        return;

    // Directory should never be released unless all its children
    // have been removed
    assert(it->second.openCount == 0);

    // Call on release handlers
    m_onRelease(uuid);

    // If the file has been marked as deleted before it was closed,
    // remove it now from the cache, otherwise put it at the end of
    // lru list and call prune
    if (it->second.deleted) {
        m_lruDirectoryData.erase(it);
        MetadataCache::erase(uuid);
    }
    else {
        it->second.lruIt =
            m_lruDirectoryList.emplace(m_lruDirectoryList.end(), uuid);
        prune();
    }
}

FileAttrPtr LRUMetadataCache::getAttr(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto attr = MetadataCache::getAttr(uuid);
    if (attr->parentUuid())
        noteDirectoryActivity(*attr->parentUuid());

    return attr;
}

FileAttrPtr LRUMetadataCache::getAttr(
    const folly::fbstring &parentUuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name);

    auto attr = MetadataCache::getAttr(parentUuid, name);
    if (attr->parentUuid())
        noteDirectoryActivity(*attr->parentUuid());

    return attr;
}

void LRUMetadataCache::putAttr(std::shared_ptr<FileAttr> attr)
{
    LOG_FCALL();

    MetadataCache::putAttr(attr);
}

void LRUMetadataCache::noteDirectoryActivity(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto res = m_lruDirectoryData.emplace(uuid, LRUData{});
    auto newEntry = res.second;

    if (newEntry) {
        res.first->second.lruIt =
            m_lruDirectoryList.emplace(m_lruDirectoryList.end(), uuid);
    }
    else if (res.first->second.lruIt) {
        // Move the entry to the end of the LRU list
        m_lruDirectoryList.splice(m_lruDirectoryList.end(), m_lruDirectoryList,
            *res.first->second.lruIt);
    }

    rest.first->second.touch();

    prune();
}

void LRUMetadataCache::prune()
{
    LOG_FCALL();

    // if (MetadataCache::size() > m_targetSize) {
    LOG(ERROR) << "MetadataCache size is larger than requested maximum size: "
               << MetadataCache::size();
    // Invalidate all directories and their direct children which are
    // expired and do not contain any opened files
    while (m_lruDirectoryList.size()) {
        LOG(ERROR) << "Directory LRU list size is: "
                   << m_lruDirectoryList.size();
        auto &uuid = m_lruDirectoryList.front();
        auto oldestItem = m_lruDirectoryData.find(uuid);

        if (oldestItem == m_lruDirectoryData.end()) {
            m_lruDirectoryList.pop_front();
            continue;
        }

        LOG(ERROR) << "Latest activity in directory " << uuid << " was "
                   << std::chrono::duration_cast<std::chrono::seconds>(
                          std::chrono::system_clock::now() -
                          oldestItem->second.lastUsed)
                          .count()
                   << " seconds ago";

        if (oldestItem->second.expired()) {
            if (oldestItem->second.openCount > 0)
                continue;

            auto uuid = std::move(m_lruDirectoryList.front());
            LOG(ERROR) << "Removing directory " << uuid
                       << " from metadata cache";
            m_lruDirectoryList.pop_front();
            m_lruDirectoryData.erase(uuid);
            MetadataCache::erase(uuid);
            m_onDropDirectory(uuid);

            // TODO: Update attributes of all opened files from dropped
            // directory
            MetadataCache::invalidateChildren(uuid);
        }
        else
            break;
    }
    //}

    /*
     *if (m_lruData.size() > m_targetSize && !m_lruList.empty()) {
     *    LOG_DBG(1) << "Pruning LRU metadata cache front because it exceeds "
     *                  "target size ("
     *               << m_lruData.size() << ">" << m_targetSize << ")";
     *    auto uuid = std::move(m_lruList.front());
     *    m_lruList.pop_front();
     *    m_lruData.erase(uuid);
     *    MetadataCache::erase(uuid);
     *    m_onPrune(uuid);
     *}
     */
}

bool LRUMetadataCache::rename(folly::fbstring uuid,
    folly::fbstring newParentUuid, folly::fbstring newName,
    folly::fbstring newUuid)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newParentUuid)
                << LOG_FARG(newName) << LOG_FARG(newUuid);

    noteDirectoryActivity(newParentUuid);
    return MetadataCache::rename(uuid, newParentUuid, newName, newUuid);
}

void LRUMetadataCache::truncate(folly::fbstring uuid, const std::size_t newSize)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newSize);

    auto attr = MetadataCache::getAttr(uuid);
    if (attr->parentUuid())
        noteDirectoryActivity(*attr->parentUuid());

    MetadataCache::truncate(uuid, newSize);
}

void LRUMetadataCache::updateTimes(
    folly::fbstring uuid, const messages::fuse::UpdateTimes &updateTimes)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto attr = MetadataCache::getAttr(uuid);
    if (attr->parentUuid())
        noteDirectoryActivity(*attr->parentUuid());

    MetadataCache::updateTimes(uuid, updateTimes);
}

void LRUMetadataCache::changeMode(
    const folly::fbstring &uuid, const mode_t newMode)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newMode);

    auto attr = MetadataCache::getAttr(uuid);
    if (attr->parentUuid())
        noteDirectoryActivity(*attr->parentUuid());

    MetadataCache::changeMode(uuid, newMode);
}

void LRUMetadataCache::putLocation(std::unique_ptr<FileLocation> location)
{
    LOG_FCALL();

    MetadataCache::putLocation(std::move(location));
}

std::shared_ptr<FileLocation> LRUMetadataCache::getLocation(
    const folly::fbstring &uuid, bool forceUpdate)
{
    LOG_FCALL();

    return MetadataCache::getLocation(uuid, forceUpdate);
}

bool LRUMetadataCache::updateLocation(const FileLocation &newLocation)
{
    return MetadataCache::updateLocation(newLocation);
}

bool LRUMetadataCache::updateLocation(
    const off_t start, const off_t end, const FileLocation &locationUpdate)
{
    return MetadataCache::updateLocation(start, end, locationUpdate);
}

void LRUMetadataCache::handleMarkDeleted(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto attr = getAttr(uuid);

    if (attr->type() == FileAttr::FileType::directory) {
        auto it = m_lruDirectoryData.find(uuid);
        if (it == m_lruDirectoryData.end())
            return;

        it->second.deleted = true;

        if (it->second.lruIt) {
            m_lruDirectoryList.erase(*it->second.lruIt);
            m_lruDirectoryData.erase(it);
        }
    }
    else {
        auto it = m_lruFileData.find(uuid);
        if (it == m_lruFileData.end())
            return;

        it->second.deleted = true;

        if (it->second.lruIt) {
            m_lruFileList.erase(*it->second.lruIt);
            m_lruFileData.erase(it);
        }
    }

    MetadataCache::erase(uuid);

    m_onMarkDeleted(uuid);
}

void LRUMetadataCache::handleRename(
    const folly::fbstring &oldUuid, const folly::fbstring &newUuid)
{
    LOG_FCALL() << LOG_FARG(oldUuid) << LOG_FARG(newUuid);

    auto attr = getAttr(oldUuid);
    if (attr->type() == FileAttr::FileType::directory) {
        LOG(ERROR) << "RENAMING DIRECTORY FROM " << oldUuid << " TO "
                   << newUuid;
    }
    else {
        auto it = m_lruFileData.find(oldUuid);
        if (it == m_lruFileData.end())
            return;

        auto lruData = std::move(it->second);
        m_lruFileData.erase(it);

        auto res = m_lruFileData.emplace(newUuid, LRUData{});
        if (res.second) {
            res.first->second = std::move(lruData);
            if (res.first->second.lruIt) {
                auto oldIt = *(res.first->second.lruIt);
                res.first->second.lruIt = m_lruFileList.emplace(oldIt, newUuid);
                m_lruFileList.erase(oldIt);
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
                m_lruFileList.erase(*lruData.lruIt);
        }
    }

    m_onRename(oldUuid, newUuid);
}

} // namespace cache
} // namespace client
} // namespace one
