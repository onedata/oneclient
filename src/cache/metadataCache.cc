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
#include "util/uuid.h"

#include <folly/FBVector.h>
#include <folly/Range.h>

#include <chrono>

using namespace std::literals;

namespace one {
namespace client {
namespace cache {

MetadataCache::MetadataCache(communication::Communicator &communicator,
    const std::chrono::seconds providerTimeout, folly::fbstring rootUuid,
    const std::vector<std::string> &spaceNames,
    const std::vector<std::string> &spaceIds)
    : m_communicator{communicator}
    , m_providerTimeout{providerTimeout}
    , m_rootUuid{std::move(rootUuid)}
{
    for (const auto &name : spaceNames) {
        m_whitelistedSpaceNames.emplace(name);
    }
    for (const auto &id : spaceIds) {
        m_whitelistedSpaceIds.emplace(id);
    }
}

void MetadataCache::setReaddirCache(std::shared_ptr<ReaddirCache> readdirCache)
{
    m_readdirCache = readdirCache;
}

bool MetadataCache::contains(const folly::fbstring &uuid) const
{
    assertInFiber();

    auto &index = bmi::get<ByUuid>(m_cache);
    return index.find(uuid) != index.end();
}

bool MetadataCache::isDeleted(const folly::fbstring &uuid) const
{
    assertInFiber();

    return m_deletedUuids.find(uuid) != m_deletedUuids.end();
}

bool MetadataCache::isSpaceWhitelisted(const FileAttr &space)
{
    LOG_FCALL() << LOG_FARG(space.name());

    assertInFiber();

    if (m_whitelistedSpaceNames.empty() && m_whitelistedSpaceIds.empty())
        return true;

    folly::fbstring spaceId = util::uuid::uuidToSpaceId(space.uuid());

    bool spaceIsWhitelistedByName =
        m_whitelistedSpaceNames.find(space.name()) !=
        m_whitelistedSpaceNames.end();

    bool spaceIsWhitelistedById =
        m_whitelistedSpaceIds.find(spaceId) != m_whitelistedSpaceIds.end();

    LOG_DBG(2) << "Space " << space.name() << "(" << spaceId << ") is "
               << spaceIsWhitelistedByName << ":" << spaceIsWhitelistedById;

    return spaceIsWhitelistedByName || spaceIsWhitelistedById;
}

void MetadataCache::invalidateChildren(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    assert(!uuid.empty());

    LOG_DBG(2) << "Invalidating children of directory: " << uuid;

    auto &index = bmi::get<ByParent>(m_cache);
    auto irange = boost::make_iterator_range(index.equal_range(uuid));
    index.erase(irange.begin(), irange.end());
}

folly::fbvector<folly::fbstring> MetadataCache::readdir(
    const folly::fbstring &uuid, off_t off, std::size_t chunkSize)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(off) << LOG_FARG(chunkSize);

    assertInFiber();

    assert(!uuid.empty());

    folly::fbvector<folly::fbstring> result;
    if (off == 0) {
        result.emplace_back(".");
        result.emplace_back("..");
    }

    auto &index = bmi::get<ByParent>(m_cache);
    auto irange = boost::make_iterator_range(index.equal_range(uuid));

    if (uuid != m_rootUuid) {
        // Advance the iterator to off safely
        off_t offCount{0};
        auto it = irange.begin();
        for (; (offCount < off - 2) && (it != irange.end()); it++, offCount++) {
        }
        if (offCount < off - 2)
            return result;

        for (size_t count = (off > 0) ? 0 : 2;
             (it != irange.end()) && (count < chunkSize); it++, count++) {
            result.emplace_back(it->attr->name());
        }
    }
    else {
        // Handle space whitelisting
        folly::fbvector<folly::fbstring> whitelistedSpaces;
        for (const auto &m : irange)
            if (isSpaceWhitelisted(*m.attr))
                whitelistedSpaces.emplace_back(m.attr->name());

        off_t offCount{0};
        auto it = whitelistedSpaces.begin();
        for (; (offCount < off - 2) && (it != whitelistedSpaces.end());
             it++, offCount++) {
        }
        if (offCount < off - 2)
            return result;

        for (size_t count = (off > 0) ? 0 : 2;
             (it != whitelistedSpaces.end()) && (count < chunkSize);
             it++, count++) {
            result.emplace_back(*it);
        }
    }

    return result;
}

std::shared_ptr<FileAttr> MetadataCache::getAttr(const folly::fbstring &uuid)
{
    assertInFiber();

    return getAttrIt(uuid)->attr;
}

FileAttrPtr MetadataCache::getAttr(
    const folly::fbstring &parentUuid, const folly::fbstring &name)
{

    LOG_FCALL() << LOG_FARG(parentUuid) << LOG_FARG(name);

    assertInFiber();

    auto &index = bmi::get<ByParentName>(m_cache);
    auto it = index.find(std::make_tuple(parentUuid, name));
    if (it != index.end() && !it->deleted) {
        LOG_DBG(2) << "Found metadata attr for file " << name
                   << " in directory " << parentUuid;

        if (it->attr->type() == FileAttr::FileType::regular &&
            !it->attr->size()) {
            LOG_DBG(2) << "Metadata for file " << parentUuid << "/" << name
                       << " exists, but size is undefined, fetch the "
                          "attribute again";
        }
        else if (parentUuid == m_rootUuid && !isSpaceWhitelisted(*it->attr)) {
            throw std::system_error(
                std::make_error_code(std::errc::no_such_file_or_directory));
        }
        else {
            return it->attr;
        }
    }

    LOG_DBG(2) << "Metadata attr for file " << name << " in directory "
               << parentUuid << " not found in cache - retrieving from server";

    auto fetchedIt = fetchAttr(messages::fuse::GetChildAttr{parentUuid, name});

    if (parentUuid == m_rootUuid && !isSpaceWhitelisted(*fetchedIt->attr)) {
        throw std::system_error(
            std::make_error_code(std::errc::no_such_file_or_directory));
    }

    LOG_DBG(2) << "Got metadata attr for file " << name << " in directory "
               << parentUuid << " from server";

    return fetchedIt->attr;
}

bool MetadataCache::putAttr(std::shared_ptr<FileAttr> attr)
{
    LOG_FCALL() << LOG_FARG(attr->toString());

    assertInFiber();

    assert(attr->parentUuid());

    auto const &uuid = attr->uuid();

    if (m_deletedUuids.find(uuid) != m_deletedUuids.end()) {
        LOG(WARNING) << "Received attribute for deleted file or directory "
                     << uuid << " - ignoring";
        return false;
    }

    try {
        if (attr->type() == FileAttr::FileType::regular &&
            !(attr->size().hasValue())) {
            LOG(WARNING)
                << "Received attribute for new file " << uuid
                << " without size - fetching full attribute from server...";

            fetchAttr(messages::fuse::GetFileAttr{uuid});
            return true;
        }

        auto result = m_cache.emplace(attr);
        auto isNewEntry = result.second;

        if (!isNewEntry) {
            LOG_DBG(2) << "File " << attr->uuid()
                       << " already exists in the cache - ignoring";
            return false;
        }

        LOG_DBG(2) << "Added new attribute to the metadata cache for: "
                   << attr->uuid();

        if (attr->parentUuid() && !attr->parentUuid().value().empty()) {
            LOG_DBG(2)
                << "Subscribing for changes on the parent of newly added file: "
                << attr->uuid();
            m_onAdd(attr->parentUuid().value());
        }

        ONE_METRIC_COUNTER_INC("comp.oneclient.mod.metadatacache.size");
        return isNewEntry;
    }
    catch (std::system_error &e) {
        if (e.code().value() == ENOENT) {
            LOG(WARNING)
                << "Trying to update attribute for file which does not "
                   "exist on the server: "
                << uuid << " - ignoring...";
            return false;
        }
        throw;
    }
    catch (std::exception &e) {
        throw;
    }
}

MetadataCache::Map::iterator MetadataCache::getAttrIt(
    const folly::fbstring &uuid)
{
    assertInFiber();

    if (m_deletedUuids.find(uuid) != m_deletedUuids.end())
        throw std::system_error(
            std::make_error_code(std::errc::no_such_file_or_directory));

    auto &index = bmi::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it != index.end()) {
        LOG_DBG(2) << "Metadata attr for file " << uuid << " found in cache";

        if (it->attr->type() == FileAttr::FileType::regular &&
            !it->attr->size()) {
            LOG_DBG(2) << "Metadata for file " << uuid
                       << " exists, but size is undefined, fetch the "
                          "attribute again";
        }
        else {
            return it;
        }
    }

    LOG_DBG(2) << "Metadata attributes for " << uuid
               << " not found in cache - fetching from server";

    auto res = fetchAttr(messages::fuse::GetFileAttr{uuid});

    LOG_DBG(2) << "Got metadata attr for " << uuid << " from server";

    return res;
}

template <typename ReqMsg>
MetadataCache::Map::iterator MetadataCache::fetchAttr(ReqMsg &&msg)
{
    LOG_FCALL();

    assertInFiber();

    auto attr = communication::wait(
        m_communicator.communicate<FileAttr>(std::forward<ReqMsg>(msg)),
        m_providerTimeout);

    if (!attr.size()) {
        LOG(ERROR) << "Received invalid message from server when fetching "
                      "attribute - size is unset.";
        throw std::errc::protocol_error; // NOLINT
    }

    auto uuid = attr.uuid();
    auto parentUuid = attr.parentUuid();

    auto sharedAttr = std::make_shared<FileAttr>(std::move(attr));
    auto result = m_cache.emplace(sharedAttr);

    LOG_DBG(2) << "Got attribute for file: " << uuid
               << " with parent UUID: " << parentUuid.value();

    if (!result.second) {
        LOG_DBG(2) << "Updating fetched attribute in cache: " << uuid;

        m_cache.modify(result.first, [a = std::move(sharedAttr)](Metadata &m) {
            m.attr = std::move(a);
        });
    }
    else {
        LOG_DBG(2) << "Added new fetched attribute to cache: " << uuid;

        // In case the parent of uuid is not in the cache, add it and subscribe
        // for change events on that directory
        if (parentUuid && !parentUuid.value().empty()) {
            m_onAdd(parentUuid.value());
        }

        ONE_METRIC_COUNTER_INC("comp.oneclient.mod.metadatacache.size");
    }

    LOG_DBG(2) << "fetchAttr for " << uuid << " complete...";

    return result.first;
}

std::shared_ptr<FileLocation> MetadataCache::getLocation(
    const folly::fbstring &uuid, bool forceUpdate)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    return getLocationPtr(getAttrIt(uuid), forceUpdate);
}

std::shared_ptr<FileLocation> MetadataCache::getLocationPtr(
    const Map::iterator &it, bool forceUpdate)
{
    LOG_FCALL();

    assertInFiber();

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

std::shared_ptr<FileLocation> MetadataCache::getLocation(
    std::shared_ptr<FileAttr> attr)
{
    LOG_FCALL() << LOG_FARG(attr->uuid());

    assertInFiber();

    return fetchFileLocation(attr->uuid());
}

std::shared_ptr<FileLocation> MetadataCache::fetchFileLocation(
    const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    auto location = communication::wait(
        m_communicator.communicate<FileLocation>(
            messages::fuse::GetFileLocation{uuid.toStdString()}),
        m_providerTimeout);

    auto sharedLocation = std::make_shared<FileLocation>(std::move(location));

    auto &index = bmi::get<ByUuid>(m_cache);
    auto it = index.find(uuid);

    if (it != index.end())
        m_cache.modify(it, [&](Metadata &m) { m.location = sharedLocation; });

    return sharedLocation;
}

void MetadataCache::ensureAttrAndLocationCached(folly::fbstring uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    auto it = getAttrIt(uuid);

    getLocationPtr(it);
}

void MetadataCache::releaseFile(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    auto &index = bmi::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end())
        return;

    // Clear the file location attached to this cached attribute
    m_cache.modify(it, [&](Metadata &m) { m.location = {}; });
}

void MetadataCache::erase(folly::fbstring uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);
    assertInFiber();

    auto &index = bmi::get<ByUuid>(m_cache);
    index.erase(uuid);
    ONE_METRIC_COUNTER_SET(
        "comp.oneclient.mod.metadatacache.size", index.size());
}

void MetadataCache::truncate(folly::fbstring uuid, const std::size_t newSize)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newSize);

    assertInFiber();

    auto &index = bmi::get<ByUuid>(m_cache);
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

    assertInFiber();

    auto &index = bmi::get<ByUuid>(m_cache);
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

    assertInFiber();

    auto &index = bmi::get<ByUuid>(m_cache);
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

    assertInFiber();

    auto it = getAttrIt(location->uuid());
    m_cache.modify(
        it, [&](Metadata &m) mutable { m.location = {std::move(location)}; });
}

bool MetadataCache::markDeleted(folly::fbstring uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    m_deletedUuids.insert(uuid);

    auto &index = bmi::get<ByUuid>(m_cache);
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

    assertInFiber();

    auto uuid = it->attr->uuid();

    auto &index = bmi::get<ByUuid>(m_cache);
    index.erase(uuid);
    ONE_METRIC_COUNTER_SET(
        "comp.oneclient.mod.metadatacache.size", index.size());

    m_onMarkDeleted(uuid);
}

bool MetadataCache::rename(folly::fbstring uuid, folly::fbstring newParentUuid,
    folly::fbstring newName, folly::fbstring newUuid, bool renewSubscriptions)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(newParentUuid)
                << LOG_FARG(newName) << LOG_FARG(newUuid);

    assertInFiber();

    auto &targetIndex = bmi::get<ByParentName>(m_cache);
    auto targetIt = targetIndex.find(std::make_tuple(newParentUuid, newName));
    if (targetIt != targetIndex.end()) {
        LOG_DBG(1) << "Target file " << newName << " in " << newParentUuid
                   << " is already cached - marking as deleted";
        markDeletedIt(m_cache.project<ByUuid>(targetIt));
    }

    auto &index = bmi::get<ByUuid>(m_cache);
    auto it = index.find(uuid);

    if (uuid != newUuid && (index.count(newUuid) > 0)) {
        LOG(WARNING) << "The rename target '" << newUuid
                     << "' is already cached";
    }
    else if (it == index.end()) {
        // The attributes for the old renamed file have not been cached yet
        // just add the new attr to the cache if the parent directory of
        // the newUuid is being cached
        try {
            fetchAttr(messages::fuse::GetFileAttr{newUuid});
        }
        catch (const std::system_error &e) {
            LOG_DBG(1) << "Rename event received for removed file - ignoring: "
                       << e.what();
            return false;
        }
    }
    else {
        index.modify(it, [&](Metadata &m) {
            m.attr->setName(newName);
            m.attr->setUuid(newUuid);
            m.attr->setParentUuid(newParentUuid);
            m.location = nullptr;
        });

        LOG_DBG(2) << "Renamed file " << uuid << " to " << newName
                   << " with new uuid " << newUuid << " in " << newParentUuid;
    }

    if (uuid != newUuid)
        m_deletedUuids.insert(uuid);

    m_onAdd(newParentUuid);

    if (renewSubscriptions)
        m_onRename(uuid, newUuid);

    return true;
}

void MetadataCache::updateSizeFromRange(const folly::fbstring &uuid,
    const boost::icl::discrete_interval<off_t> range)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    auto it = bmi::get<ByUuid>(m_cache).find(uuid);
    m_cache.modify(it, [&](Metadata &m) {
        auto newSize =
            std::max<off_t>(boost::icl::last(range) + 1, *m.attr->size());

        LOG_DBG(2) << "Updating file size for " << uuid << " to " << newSize;

        m.attr->size(newSize);
    });
}

bool MetadataCache::updateAttr(std::shared_ptr<FileAttr> newAttr)
{
    LOG_FCALL() << LOG_FARG(newAttr->toString());

    assertInFiber();

    auto uuid = newAttr->uuid();

    if (m_deletedUuids.find(uuid) != m_deletedUuids.end()) {
        LOG_DBG(2) << "Update for deleted file or directory " << uuid
                   << " - ignoring";
        return false;
    }

    auto &index = bmi::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end()) {
        LOG_DBG(2) << "Attribute for " << newAttr->name() << " (" << uuid
                   << ")  not found in cache - adding";
        return putAttr(newAttr);
    }

    LOG_DBG(2) << "Updating attribute for " << uuid;

    index.modify(
        it, [&](Metadata &m) {
            if (m.attr->type() == FileAttr::FileType::regular) {
                if (newAttr->size() && m.attr->size() &&
                    (*newAttr->size() < *m.attr->size()) && m.location) {
                    LOG_DBG(2)
                        << "Truncating file size based on updated attributes "
                           "for uuid: '"
                        << uuid << "'";

                    m.location->truncate(
                        boost::icl::discrete_interval<off_t>::right_open(
                            0, *newAttr->size()));
                }
                if (newAttr->size())
                    m.attr->size(*newAttr->size());
            }

            m.attr->atime(std::max(m.attr->atime(), newAttr->atime()));
            m.attr->ctime(std::max(m.attr->ctime(), newAttr->ctime()));
            m.attr->mtime(std::max(m.attr->mtime(), newAttr->mtime()));

            m.attr->gid(newAttr->gid());
            m.attr->mode(newAttr->mode());
            m.attr->uid(newAttr->uid());
        });

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
