/**
 * @file metadataCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "metadataCache.h"
#include "fuseOperations.h"

#include "logging.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/getChildAttr.h"
#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/rename.h"
#include "messages/fuse/updateTimes.h"
#include "scheduler.h"

#include <folly/FBVector.h>
#include <folly/Range.h>

#include <chrono>

using namespace std::literals;

namespace one {
namespace client {
namespace cache {

MetadataCache::MetadataCache(communication::Communicator &communicator)
    : m_communicator{communicator}
{
}

FileAttrPtr MetadataCache::getAttr(const folly::fbstring &uuid)
{
    return getAttrIt(uuid)->attr;
}

FileAttrPtr MetadataCache::getAttr(
    const folly::fbstring &parentUuid, const folly::fbstring &name)
{
    DLOG(INFO) << "Fetching attributes for child '" << name << "' of '"
               << parentUuid << "'";

    auto &index = boost::multi_index::get<ByParent>(m_cache);
    auto it = index.find(std::make_tuple(parentUuid, name));
    if (it != index.end() && !it->deleted)
        return it->attr;

    if (it != index.end() && it->deleted) {
        LOG(WARNING) << "Lookup ('" << parentUuid << "', '" << name
                     << "') found a deleted file";

        index.modify(it, [&](Metadata &m) { m.attr->setParentUuid(""); });
    }

    auto fetchedIt = fetchAttr(
        messages::fuse::GetChildAttr{std::move(parentUuid), std::move(name)});

    return fetchedIt->attr;
}

MetadataCache::Map::iterator MetadataCache::getAttrIt(
    const folly::fbstring &uuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it != index.end())
        return it;

    DLOG(INFO) << "Fetching attributes for '" << uuid << "'";
    return fetchAttr(messages::fuse::GetFileAttr{uuid});
}

void MetadataCache::addBlock(const folly::fbstring &uuid,
    const helpers::FlagsSet & /*flagsSet*/,
    const boost::icl::discrete_interval<off_t> range,
    messages::fuse::FileBlock fileBlock)
{
    auto it = getAttrIt(uuid);
    auto newBlock = std::make_pair(range, std::move(fileBlock));

    // TODO: VFS-2679 Differing blocks on different locations doesn't work now
    for (auto locElem : it->locations)
        locElem.second->blocks() += newBlock;

    m_cache.modify(it, [&](Metadata &m) {
        m.attr->size(
            std::max<off_t>(boost::icl::last(range) + 1, *m.attr->size()));
    });
}

template <typename ReqMsg>
MetadataCache::Map::iterator MetadataCache::fetchAttr(ReqMsg &&msg)
{
    auto attr = communication::wait(
        m_communicator.communicate<FileAttr>(std::forward<ReqMsg>(msg)));

    if (!attr.size())
        throw std::errc::protocol_error;

    auto sharedAttr = std::make_shared<FileAttr>(std::move(attr));
    auto result = m_cache.emplace(sharedAttr);
    if (!result.second)
        m_cache.modify(result.first, [&](Metadata &m) { m.attr = sharedAttr; });

    return result.first;
}

std::shared_ptr<FileLocation> MetadataCache::fetchFileLocation(
    const folly::fbstring &uuid, const helpers::FlagsSet &flagsSet)
{
    auto location =
        communication::wait(m_communicator.communicate<FileLocation>(
            messages::fuse::GetFileLocation{uuid.toStdString(), flagsSet}));

    auto sharedLocation = std::make_shared<FileLocation>(std::move(location));

    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    assert(it != index.end());

    const auto flag = getFlagForLocation(flagsSet);
    m_cache.modify(it, [&](Metadata &m) {
        auto res = m.locations.emplace(flag, sharedLocation);
        if (!res.second)
            res.first->second = sharedLocation;
    });

    return sharedLocation;
}

FileLocationPtr MetadataCache::getFileLocation(
    const folly::fbstring &uuid, const helpers::FlagsSet &flagsSet)
{
    auto it = getAttrIt(uuid);
    return getLocationPtr(it, flagsSet);
}

std::shared_ptr<FileLocation> MetadataCache::getLocationPtr(
    const Map::iterator &it, const helpers::FlagsSet &flagsSet)
{
    const auto flag = getFlagForLocation(flagsSet);
    auto locIt = it->locations.find(flag);
    if (locIt != it->locations.end())
        return locIt->second;

    return fetchFileLocation(it->attr->uuid(), flagsSet);
}

folly::Optional<folly::fbstring> MetadataCache::getHandleId(
    const folly::fbstring &uuid, const helpers::FlagsSet &flagsSet)
{
    auto it = getAttrIt(uuid);
    auto location = getLocationPtr(it, flagsSet);
    auto handleId = location->handleId();
    location->unsetHandleId();
    return handleId;
}

folly::Optional<
    std::pair<boost::icl::discrete_interval<off_t>, messages::fuse::FileBlock>>
MetadataCache::getBlock(const folly::fbstring &uuid,
    const helpers::FlagsSet &flagsSet, const off_t offset)
{
    auto location = getFileLocation(uuid, flagsSet);
    auto availableBlockIt =
        location->blocks().find(boost::icl::discrete_interval<off_t>(offset));

    if (availableBlockIt != location->blocks().end())
        return std::make_pair(
            availableBlockIt->first, availableBlockIt->second);

    return {};
}

void MetadataCache::erase(const folly::fbstring &uuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    index.erase(uuid);
}

void MetadataCache::truncate(
    const folly::fbstring &uuid, const std::size_t newSize)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end())
        return;

    index.modify(it, [&](Metadata &m) {
        m.attr->size(newSize);
        for (auto &locElem : m.locations) {
            locElem.second->blocks() &=
                boost::icl::discrete_interval<off_t>::right_open(0, newSize);
        }
    });
}

void MetadataCache::updateTimes(
    const folly::fbstring &uuid, const messages::fuse::UpdateTimes &updateTimes)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end())
        return;

    index.modify(it, [&](Metadata &m) {
        if (updateTimes.atime())
            m.attr->atime(*updateTimes.atime());
        if (updateTimes.mtime())
            m.attr->mtime(*updateTimes.mtime());
        if (updateTimes.ctime())
            m.attr->ctime(*updateTimes.ctime());
    });
}

void MetadataCache::changeMode(
    const folly::fbstring &uuid, const mode_t newMode)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end())
        return;

    index.modify(it, [&](Metadata &m) { m.attr->mode(newMode); });
}

void MetadataCache::putLocation(
    const helpers::Flag &flag, std::unique_ptr<FileLocation> location)
{
    auto it = getAttrIt(location->uuid());
    m_cache.modify(it, [&](Metadata &m) mutable {
        m.locations[flag] = {std::move(location)};
    });
}

bool MetadataCache::markDeleted(const folly::fbstring &uuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end())
        return false;

    m_cache.modify(it, [&](Metadata &m) {
        m.attr->setParentUuid("");
        m.deleted = true;
    });

    m_onMarkDeleted(uuid);
    return true;
}

bool MetadataCache::rename(const folly::fbstring &uuid,
    const folly::fbstring &newParentUuid, const folly::fbstring &newName,
    const folly::fbstring &newUuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end())
        return false;

    if (uuid != newUuid && index.count(newUuid)) {
        LOG(WARNING) << "The rename target '" << newUuid
                     << "' is already cached";

        m_cache.erase(it);
    }
    else {
        index.modify(it, [&](Metadata &m) {
            m.attr->setName(newName);
            m.attr->setUuid(newUuid);
            m.attr->setParentUuid(newParentUuid);
            m.locations.clear();
        });
    }

    m_onRename(uuid, newUuid);

    return true;
}

bool MetadataCache::updateFileAttr(const FileAttr &newAttr)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(newAttr.uuid());
    if (it == index.end())
        return false;

    index.modify(it, [&](Metadata &m) {
        if (newAttr.size() && *newAttr.size() < *m.attr->size() &&
            !m.locations.empty()) {
            LOG(INFO) << "Truncating blocks attributes for uuid: '"
                      << newAttr.uuid() << "'";

            for (auto &locElem : m.locations) {
                locElem.second->blocks() &=
                    boost::icl::discrete_interval<off_t>::right_open(
                        0, *newAttr.size());
            }
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

bool MetadataCache::updateFileLocation(const FileLocation &newLocation)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(newLocation.uuid());
    if (it == index.end() || it->locations.empty())
        return false;

    // TODO: VFS-2679 Which location to update?

    for (auto &locElem : it->locations) {
        locElem.second->storageId(newLocation.storageId());
        locElem.second->fileId(newLocation.fileId());
        locElem.second->blocks() = newLocation.blocks();
    }

    auto loc = it->locations.begin()->second;
    auto prIts = m_updatePromises.equal_range(newLocation.uuid());

    folly::fbvector<std::shared_ptr<folly::Promise<FileLocationPtr>>>
        promisesToFulfill;

    for (auto prIt = prIts.first; prIt != prIts.second; ++prIt)
        promisesToFulfill.emplace_back(prIt->second);

    m_updatePromises.erase(prIts.first, prIts.second);

    for (auto &p : promisesToFulfill)
        p->setValue(loc);

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
} // namespace one
} // namespace client
