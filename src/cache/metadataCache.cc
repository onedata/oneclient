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

void MetadataCache::putAttr(std::shared_ptr<FileAttr> attr)
{
    auto result = m_cache.emplace(attr);
    if (!result.second)
        m_cache.modify(result.first, [&](Metadata &m) { m.attr = attr; });
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
    const boost::icl::discrete_interval<off_t> range,
    messages::fuse::FileBlock fileBlock)
{
    auto it = getAttrIt(uuid);
    auto newBlock = std::make_pair(range, std::move(fileBlock));

    assert(it->location);
    it->location->blocks() += newBlock;

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

std::shared_ptr<FileLocation> MetadataCache::getLocationPtr(
    const Map::iterator &it)
{
    if (it->location)
        return it->location;

    return fetchFileLocation(it->attr->uuid());
}

std::shared_ptr<FileLocation> MetadataCache::fetchFileLocation(
    const folly::fbstring &uuid)
{
    auto location =
        communication::wait(m_communicator.communicate<FileLocation>(
            messages::fuse::GetFileLocation{uuid.toStdString()}));

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

void MetadataCache::ensureAttrAndLocationCached(const folly::fbstring &uuid)
{
    auto it = getAttrIt(uuid);
    getLocationPtr(it);
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
        if (m.location)
            m.location->blocks() &=
                boost::icl::discrete_interval<off_t>::right_open(0, newSize);
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

void MetadataCache::putLocation(std::unique_ptr<FileLocation> location)
{
    auto it = getAttrIt(location->uuid());
    m_cache.modify(
        it, [&](Metadata &m) mutable { m.location = {std::move(location)}; });
}

bool MetadataCache::markDeleted(const folly::fbstring &uuid)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(uuid);
    if (it == index.end())
        return false;

    markDeletedIt(it);
    return true;
}

void MetadataCache::markDeletedIt(const Map::iterator &it)
{
    m_cache.modify(it, [&](Metadata &m) {
        m.attr->setParentUuid("");
        m.deleted = true;
    });

    m_onMarkDeleted(it->attr->uuid());
}

bool MetadataCache::rename(const folly::fbstring &uuid,
    const folly::fbstring &newParentUuid, const folly::fbstring &newName,
    const folly::fbstring &newUuid)
{
    auto &targetIndex = boost::multi_index::get<ByParent>(m_cache);
    auto targetIt = targetIndex.find(std::make_tuple(newParentUuid, newName));
    if (targetIt != targetIndex.end())
        markDeletedIt(m_cache.project<ByUuid>(targetIt));

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
            m.location = nullptr;
        });
    }

    m_onRename(uuid, newUuid);

    return true;
}

bool MetadataCache::updateAttr(const FileAttr &newAttr)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(newAttr.uuid());
    if (it == index.end())
        return false;

    index.modify(it, [&](Metadata &m) {
        if (newAttr.size() && *newAttr.size() < *m.attr->size() && m.location) {
            LOG(INFO) << "Truncating blocks attributes for uuid: '"
                      << newAttr.uuid() << "'";

            m.location->blocks() &=
                boost::icl::discrete_interval<off_t>::right_open(
                    0, *newAttr.size());
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

bool MetadataCache::updateLocation(const FileLocation &newLocation)
{
    auto &index = boost::multi_index::get<ByUuid>(m_cache);
    auto it = index.find(newLocation.uuid());
    if (it == index.end() || !it->location)
        return false;

    it->location->storageId(newLocation.storageId());
    it->location->fileId(newLocation.fileId());
    it->location->blocks() = newLocation.blocks();

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
