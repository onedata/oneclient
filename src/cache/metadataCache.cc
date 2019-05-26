/**
 * @file metadataCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "metadataCache.h"

#include "cache/readdirCache.h"
#include "fuseOperations.h"
#include "helpers/logging.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/getChildAttr.h"
#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/updateTimes.h"
#include "monitoring/monitoring.h"
#include "scheduler.h"
#include "util/base64.h"

#include <folly/FBVector.h>
#include <folly/Range.h>

#include <chrono>

using namespace std::literals;

namespace one {
namespace client {
namespace cache {

MetadataCache::MetadataCache(communication::Communicator &communicator,
    const std::chrono::seconds providerTimeout)
    : m_communicator{communicator}
    , m_providerTimeout{providerTimeout}
{
}

void MetadataCache::setReaddirCache(std::shared_ptr<ReaddirCache> readdirCache)
{
    m_readdirCache = readdirCache;
}

FileAttrPtr MetadataCache::getAttr(const folly::fbstring &uuid)
{
    return getAttrIt(uuid)->attr;
}

FileAttrPtr MetadataCache::getAttr(
    const folly::fbstring &parentUuid, const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name);
    LOG_DBG(2) << "Fetching attributes for child '" << name << "' of '"
               << parentUuid << "'";

    auto &index = boost::multi_index::get<ByParent>(m_cache);
    auto it = index.find(std::make_tuple(parentUuid, name));
    if (it != index.end() && !it->deleted) {
        LOG_DBG(2) << "Found metadata attr for file " << name
                   << " in directory " << parentUuid;
        return it->attr;
    }

    if (it != index.end() && it->deleted) {
        LOG(WARNING) << "Lookup ('" << parentUuid << "', '" << name
                     << "') found a deleted file";

        index.modify(it, [&](Metadata &m) { m.attr->setParentUuid(""); });
    }

    LOG_DBG(2) << "Metadata attr for file " << name << " in directory "
               << parentUuid << " not found in cache - retrieving from server";

    auto fetchedIt = fetchAttr(messages::fuse::GetChildAttr{parentUuid, name});

    LOG_DBG(2) << "Got metadata attr for file " << name << " in directory "
               << parentUuid << " from server";

    return fetchedIt->attr;
}

void MetadataCache::putAttr(std::shared_ptr<FileAttr> attr)
{
    LOG_FCALL() << LOG_FARG(attr->toString());

    auto result = m_cache.emplace(attr);
    if (!result.second)
        m_cache.modify(result.first, [&](Metadata &m) { m.attr = attr; });
    else
        ONE_METRIC_COUNTER_INC("comp.oneclient.mod.metadatacache.size");
}

MetadataCache::Map::iterator MetadataCache::getAttrIt(
    const folly::fbstring &uuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it != index.end()) {
        LOG_DBG(2) << "Metadata attr for file " << uuid << " found in cache";
        return it;
    }

    LOG_DBG(2) << "Metadata attributes for " << uuid
               << " not found in cache - fetching from server";

    auto res = fetchAttr(messages::fuse::GetFileAttr{uuid});

    LOG_DBG(2) << "Got metadata attr for " << uuid << " from server";

    return res;
}

void MetadataCache::addBlock(const folly::fbstring &uuid,
    const boost::icl::discrete_interval<off_t> range,
    messages::fuse::FileBlock fileBlock)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(range)
                << LOG_FARG(fileBlock.fileId())
                << LOG_FARG(fileBlock.storageId());

    auto it = getAttrIt(uuid);
    auto newBlock = std::make_pair(range, std::move(fileBlock));

    assert(it->location);

    it->location->putBlock(newBlock);

    LOG_DBG(2) << "Updated file " << uuid
               << " location range with new block: " << range;

    m_cache.modify(it, [&](Metadata &m) {
        m.attr->size(
            std::max<off_t>(boost::icl::last(range) + 1, *m.attr->size()));
    });
}

template <typename ReqMsg>
MetadataCache::Map::iterator MetadataCache::fetchAttr(ReqMsg &&msg)
{
    LOG_FCALL();

    LOG_DBG(2) << "Fetching attribute for metadata cache";

    auto attr = communication::wait(
        m_communicator.communicate<FileAttr>(std::forward<ReqMsg>(msg)),
        m_providerTimeout);

    if (!attr.size()) {
        LOG(ERROR)
            << "Received invalid message from server when fetching attribute.";
        throw std::errc::protocol_error; // NOLINT
    }

    auto sharedAttr = std::make_shared<FileAttr>(std::move(attr));
    auto result = m_cache.emplace(sharedAttr);
    if (!result.second)
        m_cache.modify(result.first, [&](Metadata &m) { m.attr = sharedAttr; });
    else
        ONE_METRIC_COUNTER_INC("comp.oneclient.mod.metadatacache.size");

    return result.first;
}

std::shared_ptr<FileLocation> MetadataCache::getLocation(
    const folly::fbstring &uuid, bool forceUpdate)
{
    LOG_FCALL() << LOG_FARG(uuid);

    return getLocationPtr(getAttrIt(uuid), forceUpdate);
}

std::shared_ptr<FileLocation> MetadataCache::getLocationPtr(
    const Map::iterator &it, bool forceUpdate)
{
    LOG_FCALL();

    if (!forceUpdate && it->location) {
        LOG_DBG(2) << "Found file location in metadata cache for "
                   << it->attr->uuid();
        return it->location;
    }

    LOG_DBG(2) << "File location not found in metadata cache or forced update "
                  "requested for "
               << it->attr->uuid() << " - fetching from server";

    auto res = fetchFileLocation(it->attr->uuid());

    LOG_DBG(2) << "Received file location from server for " << it->attr->uuid();

    return res;
}

std::shared_ptr<FileLocation> MetadataCache::fetchFileLocation(
    const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto location = communication::wait(
        m_communicator.communicate<FileLocation>(
            messages::fuse::GetFileLocation{uuid.toStdString()}),
        m_providerTimeout);

    auto sharedLocation = std::make_shared<FileLocation>(std::move(location));

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    assert(it != index.end());

    m_cache.modify(it, [&](Metadata &m) { m.location = sharedLocation; });

    return sharedLocation;
}

folly::Optional<
    std::pair<boost::icl::discrete_interval<off_t>, messages::fuse::FileBlock>>
MetadataCache::getBlock(const folly::fbstring &uuid, const off_t offset)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(offset);

    auto it = getAttrIt(uuid);
    auto location = getLocationPtr(it);
    auto availableBlockIt =
        location->blocks().find(boost::icl::discrete_interval<off_t>(offset));

    if (availableBlockIt != location->blocks().end())
        return std::make_pair(
            availableBlockIt->first, availableBlockIt->second);

    return {};
}

messages::fuse::FileBlock MetadataCache::getDefaultBlock(
    const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto it = getAttrIt(uuid);
    auto location = getLocationPtr(it);
    return messages::fuse::FileBlock{location->storageId(), location->fileId()};
}

const std::string &MetadataCache::getSpaceId(const folly::fbstring &uuid)
{
    auto it = getAttrIt(uuid);
    auto location = getLocationPtr(it);
    return location->spaceId();
}

void MetadataCache::ensureAttrAndLocationCached(folly::fbstring uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto it = getAttrIt(uuid);
    getLocationPtr(it);
}

void MetadataCache::erase(folly::fbstring uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    index.erase(uuid);
    ONE_METRIC_COUNTER_SET(
        "comp.oneclient.mod.metadatacache.size", index.size());
}

void MetadataCache::truncate(folly::fbstring uuid, const std::size_t newSize)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newSize);

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end()) {
        LOG_DBG(1) << "Truncate failed - file " << uuid
                   << " not found in metadata cache";
        return;
    }

    index.modify(it, [&](Metadata &m) {
        m.attr->size(newSize);
        if (m.location)
            m.location->truncate(
                boost::icl::discrete_interval<off_t>::right_open(0, newSize));
    });
}

void MetadataCache::updateTimes(
    folly::fbstring uuid, const messages::fuse::UpdateTimes &updateTimes)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(updateTimes.toString());
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end()) {
        LOG_DBG(1) << "Update times failed - file " << uuid
                   << " not found in metadata cache";
        return;
    }

    index.modify(it, [&](Metadata &m) {
        if (updateTimes.atime()) {
            LOG_DBG(2) << "Updating atime to for " << uuid << " to "
                       << std::chrono::system_clock::to_time_t(
                              *updateTimes.atime());
            m.attr->atime(*updateTimes.atime());
        }
        if (updateTimes.mtime()) {
            LOG_DBG(2) << "Updating mtime to for " << uuid << " to "
                       << std::chrono::system_clock::to_time_t(
                              *updateTimes.mtime());
            m.attr->mtime(*updateTimes.mtime());
        }
        if (updateTimes.ctime()) {
            LOG_DBG(2) << "Updating ctime to for " << uuid << " to "
                       << std::chrono::system_clock::to_time_t(
                              *updateTimes.ctime());
            m.attr->ctime(*updateTimes.ctime());
        }
    });
}

void MetadataCache::changeMode(folly::fbstring uuid, const mode_t newMode)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARGO(newMode);

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end()) {
        LOG_DBG(1) << "Change mode failed - file " << uuid
                   << " not found in metadata cache";
        return;
    }

    index.modify(it, [&](Metadata &m) { m.attr->mode(newMode); });
}

void MetadataCache::putLocation(std::unique_ptr<FileLocation> location)
{
    LOG_FCALL() << LOG_FARG(location->toString());

    auto it = getAttrIt(location->uuid());
    m_cache.modify(
        it, [&](Metadata &m) mutable { m.location = {std::move(location)}; });
}

bool MetadataCache::markDeleted(folly::fbstring uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end()) {
        LOG_DBG(1) << "Mark deleted failed - file " << uuid
                   << " not found in metadata cache";
        return false;
    }

    markDeletedIt(it);
    return true;
}

void MetadataCache::markDeletedIt(const Map::iterator &it)
{
    LOG_FCALL() << LOG_FARG(it->attr->uuid());

    auto uuid = it->attr->uuid();
    auto parentUuid = it->attr->parentUuid();

    m_cache.modify(it, [&](Metadata &m) {
        m.attr->setParentUuid("");
        m.deleted = true;
    });

    if (parentUuid)
        m_readdirCache->invalidate(*(it->attr->parentUuid()));

    m_onMarkDeleted(uuid);
}

bool MetadataCache::rename(folly::fbstring uuid, folly::fbstring newParentUuid,
    folly::fbstring newName, folly::fbstring newUuid)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newParentUuid)
                << LOG_FARG(newName) << LOG_FARG(newUuid);

    auto &targetIndex = boost::multi_index::get<ByParent>(m_cache);
    auto targetIt = targetIndex.find(std::make_tuple(newParentUuid, newName));
    if (targetIt != targetIndex.end()) {
        LOG_DBG(1) << "Target file " << newName << " in " << newParentUuid
                   << " is already cached - marking as deleted";
        markDeletedIt(m_cache.project<ByUuid>(targetIt));
    }

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end()) {
        LOG_DBG(1) << "File " << uuid
                   << " to be renamed is not in metadata cache";
        return false;
    }

    if (uuid != newUuid && (index.count(newUuid) > 0)) {
        LOG(WARNING) << "The rename target '" << newUuid
                     << "' is already cached";

        m_cache.erase(it);
    }
    else {
        index.modify(it, [&](Metadata &m) {
            m.attr->setName(newName);
            m.attr->setUuid(newUuid);
            m.attr->setParentUuid(newParentUuid);
            m.location = nullptr;
        });

        if (it->attr->parentUuid())
            m_readdirCache->invalidate(*(it->attr->parentUuid()));
        m_readdirCache->invalidate(newParentUuid);

        LOG_DBG(2) << "Renamed file " << uuid << " to " << newName
                   << " with new uuid " << newUuid << " in " << newParentUuid;
    }

    m_onRename(uuid, newUuid);

    return true;
}

bool MetadataCache::updateAttr(const FileAttr &newAttr)
{
    LOG_FCALL() << LOG_FARG(newAttr.toString());

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(newAttr.uuid());
    if (it == index.end()) {
        LOG_DBG(1) << "Update attr failed - file " << newAttr.name() << "("
                   << newAttr.uuid() << ") not found in metadata cache";
        return false;
    }

    index.modify(it, [&](Metadata &m) {
        if (newAttr.size() && *newAttr.size() < *m.attr->size() && m.location) {
            LOG_DBG(2) << "Truncating file size based on updated attributes "
                          "for uuid: '"
                       << newAttr.uuid() << "'";

            m.location->truncate(
                boost::icl::discrete_interval<off_t>::right_open(
                    0, *newAttr.size()));
        }

        m.attr->atime(std::max(m.attr->atime(), newAttr.atime()));
        m.attr->ctime(std::max(m.attr->ctime(), newAttr.ctime()));
        m.attr->mtime(std::max(m.attr->mtime(), newAttr.mtime()));
        m.attr->gid(newAttr.gid());
        m.attr->mode(newAttr.mode());
        if (newAttr.size())
            m.attr->size(*newAttr.size());
        m.attr->uid(newAttr.uid());
    });

    return true;
}

bool MetadataCache::updateLocation(
    const off_t start, const off_t end, const FileLocation &locationUpdate)
{
    LOG_FCALL() << LOG_FARG(locationUpdate.toString());

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(locationUpdate.uuid());
    if (it == index.end() || !it->location) {
        LOG_DBG(1) << "Update location failed - file " << locationUpdate.uuid()
                   << " not found in metadata cache";
        return false;
    }

    it->location->version(locationUpdate.version());
    it->location->storageId(locationUpdate.storageId());
    it->location->fileId(locationUpdate.fileId());
    it->location->updateInRange(start, end, locationUpdate);

    LOG_DBG(2) << "Updated file location for file " << locationUpdate.uuid()
               << " in range [" << start << ", " << end << ")";

    return true;
}

bool MetadataCache::updateLocation(const FileLocation &newLocation)
{
    LOG_FCALL() << LOG_FARG(newLocation.toString());

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(newLocation.uuid());
    if (it == index.end() || !it->location) {
        LOG_DBG(1) << "Update location failed - file " << newLocation.uuid()
                   << " not found in metadata cache";
        return false;
    }

    if (newLocation.version() < it->location->version()) {
        LOG(INFO) << "New file location older than current ("
                  << newLocation.version() << " < " << it->location->version()
                  << ". Aborting update.";
        return false;
    }

    it->location->version(newLocation.version());
    it->location->storageId(newLocation.storageId());
    it->location->fileId(newLocation.fileId());
    it->location->update(newLocation.blocks());

    LOG_DBG(2) << "Updated file location for file " << newLocation.uuid();

    return true;
}

MetadataCache::Metadata::Metadata(std::shared_ptr<FileAttr> attr_)
    : attr{std::move(attr_)}
{
}

auto MetadataCache::NameExtractor::operator()(const Metadata &m) const
    -> const result_type &
{
    return m.attr->name();
}

auto MetadataCache::UuidExtractor::operator()(const Metadata &m) const
    -> const result_type &
{
    return m.attr->uuid();
}

auto MetadataCache::ParentUuidExtractor::operator()(const Metadata &m) const
    -> result_type
{
    return m.attr->parentUuid() ? *m.attr->parentUuid() : folly::fbstring{};
}

} // namespace cache
} // namespace client
} // namespace one
