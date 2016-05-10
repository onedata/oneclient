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
#include "messages/fuse/fileRenamed.h"
#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/rename.h"
#include "scheduler.h"

#include <chrono>

using namespace std::literals;

namespace one {
namespace client {

MetadataCache::MetadataCache(communication::Communicator &communicator)
    : m_communicator{communicator}
{
}

MetadataCache::FileAttr MetadataCache::getAttr(const Path &path)
{
    ConstUuidAccessor constUuidAcc;
    if (m_pathToUuid.find(constUuidAcc, path))
        return getAttr(constUuidAcc->second);

    UuidAccessor uuidAcc;
    MetaAccessor metaAcc;
    getAttr(uuidAcc, metaAcc, path);
    return metaAcc->second.attr.get();
}

MetadataCache::FileAttr MetadataCache::getAttr(const std::string &uuid)
{
    ConstMetaAccessor constAcc;
    if (m_metaCache.find(constAcc, uuid)) {
        if (constAcc->second.attr)
            return constAcc->second.attr.get();

        constAcc.release();
    }

    MetaAccessor acc;
    getAttr(acc, uuid);
    return acc->second.attr.get();
}

MetadataCache::FileLocation MetadataCache::getLocation(
    const std::string &uuid, const one::helpers::FlagsSet flags)
{
    auto filteredFlags = filterFlagsForLocation(flags);

    ConstMetaAccessor constAcc;
    if (m_metaCache.find(constAcc, uuid)) {
        if (constAcc->second.locations.find(filteredFlags) !=
            constAcc->second.locations.end())
            return constAcc->second.locations.at(filteredFlags);

        constAcc.release();
    }

    MetaAccessor acc;
    getLocation(acc, uuid, flags);
    return acc->second.locations.at(filteredFlags);
}

void MetadataCache::getAttr(MetaAccessor &metaAcc, const Path &path)
{
    UuidAccessor uuidAcc;
    getAttr(uuidAcc, metaAcc, path);
}

void MetadataCache::getAttr(
    UuidAccessor &uuidAcc, MetaAccessor &metaAcc, const Path &path)
{
    if (!m_pathToUuid.insert(uuidAcc, path)) {
        getAttr(metaAcc, uuidAcc->second);
        metaAcc->second.path = path;
        return;
    }

    try {
        DLOG(INFO) << "Fetching attributes for " << path;
        auto attr = fetchAttr(messages::fuse::GetFileAttr{path});
        uuidAcc->second = attr.uuid();
        if (m_metaCache.insert(metaAcc, attr.uuid())) {
            // In this case we're fetching attributes because we didn't know
            // the path mapped to an already cached uuid. Do not update attrs
            // to avoid race conditions with our own write events.
            metaAcc->second.attr = std::move(attr);
        }
        metaAcc->second.path = path;
    }
    catch (...) {
        if (!metaAcc.empty())
            m_metaCache.erase(metaAcc);

        m_pathToUuid.erase(uuidAcc);
        throw;
    }
}

bool MetadataCache::get(MetaAccessor &metaAcc, const std::string &uuid)
{
    return m_metaCache.find(metaAcc, uuid);
}

void MetadataCache::getAttr(MetaAccessor &metaAcc, const std::string &uuid)
{
    if (!m_metaCache.insert(metaAcc, uuid)) {
        if (metaAcc->second.attr)
            return;
    }

    try {
        DLOG(INFO) << "Fetching attributes for " << uuid;
        metaAcc->second.attr = fetchAttr(messages::fuse::GetFileAttr{uuid});
    }
    catch (...) {
        m_metaCache.erase(metaAcc);
        throw;
    }
}

void MetadataCache::getLocation(MetadataCache::MetaAccessor &metaAcc,
    const std::string &uuid, const one::helpers::FlagsSet flags)
{
    auto filteredFlags = filterFlagsForLocation(flags);

    if (!m_metaCache.insert(metaAcc, uuid)) {
        if (metaAcc->second.locations.find(filteredFlags) !=
            metaAcc->second.locations.end())
            return;
    }

    try {
        DLOG(INFO) << "Fetching file location for " << uuid;
        auto future = m_communicator.communicate<FileLocation>(
            messages::fuse::GetFileLocation{uuid, flags});

        metaAcc->second.locations[filteredFlags] = communication::wait(future);
    }
    catch (...) {
        m_metaCache.erase(metaAcc);
        throw;
    }
}

std::vector<std::pair<std::string, std::string>> MetadataCache::rename(
    const MetadataCache::Path &oldPath, const MetadataCache::Path &newPath)
{
    // By convention, to avoid deadlocks, always lock on path before metadata
    UuidAccessor newUuidAcc;
    m_pathToUuid.insert(newUuidAcc, newPath);
    std::vector<std::pair<std::string, std::string>> uuidChanges;

    try {
        UuidAccessor oldUuidAcc;
        MetaAccessor oldMetaAcc;
        getAttr(oldUuidAcc, oldMetaAcc, oldPath);
        auto &oldUuid = oldMetaAcc->second.attr.get().uuid();

        DLOG(INFO) << "Renaming file " << oldUuid << " to " << newPath;

        m_pathToUuid.erase(oldUuidAcc);

        if (oldMetaAcc->second.state == FileState::removedUpstream) {
            // This rename operation was executed by fileRemovalHandler
            // Only oldPath is changed to newPath
            oldMetaAcc->second.locations.clear();
            oldMetaAcc->second.path = newPath;
            newUuidAcc->second = oldUuid;
        }
        else if (oldMetaAcc->second.state == FileState::renamedUpstream) {
            // This rename operation was executed by fileRenamedHandler
            // To inform fuse about file renaming, further modifications
            // of cache will be done in handler and file should be treated
            // normally afterwards
            oldMetaAcc->second.state = FileState::normal;
        }
        else if (oldMetaAcc->second.state == FileState::normal) {
            auto future =
                m_communicator.communicate<messages::fuse::FileRenamed>(
                    messages::fuse::Rename{oldUuid, newPath});
            auto fileRenamed = communication::wait(future);

            // Rename successful; if target exists, its metadata
            // should be removed from cache, since it has been overwritten.
            // If it was open, it has been renamed to fuse hidden file
            // before this rename operation.
            if (newUuidAcc->second != "") {
                MetaAccessor targetMetaAcc;
                if (get(targetMetaAcc, newUuidAcc->second)) {
                    m_metaCache.erase(targetMetaAcc);
                }
            }

            auto newUuid = fileRenamed.newUuid();
            remapFile(oldMetaAcc, newUuidAcc, oldUuid, newUuid, newPath);
            if (oldUuid != newUuid)
                uuidChanges.emplace_back(std::make_pair(oldUuid, newUuid));

            for (auto &childEntry : fileRenamed.childEntries()) {
                remapFile(childEntry.oldUuid(), childEntry.newUuid(),
                    childEntry.newPath());

                uuidChanges.emplace_back(
                    std::make_pair(childEntry.oldUuid(), childEntry.newUuid()));
            }
        }
    }
    catch (...) {
        m_pathToUuid.erase(newUuidAcc);
        throw;
    }

    return uuidChanges;
}

void MetadataCache::remapFile(MetaAccessor &oldMetaAcc,
    UuidAccessor &newUuidAcc, const std::string &oldUuid,
    const std::string &newUuid, const Path &newPath)
{
    newUuidAcc->second = newUuid;
    newUuidAcc.release();

    m_pathToUuid.erase(oldMetaAcc->second.path.get());
    if (newUuid != oldUuid) {
        // Copy and update old metadata if uuid has changed
        auto attr = oldMetaAcc->second.attr;
        m_metaCache.erase(oldMetaAcc);

        MetaAccessor newMetaAcc;
        m_metaCache.insert(newMetaAcc, newUuid);
        newMetaAcc->second.attr = attr;
        newMetaAcc->second.attr->uuid(newUuid);
        newMetaAcc->second.path = newPath;
    }
    else {
        // Update metadata if uuid has not changed
        oldMetaAcc->second.path = newPath;
        oldMetaAcc->second.locations.clear();
        oldMetaAcc.release();
    }
}

void MetadataCache::remapFile(
    const std::string &oldUuid, const std::string &newUuid, const Path &newPath)
{
    UuidAccessor newUuidAcc;
    m_pathToUuid.insert(newUuidAcc, newPath);
    MetaAccessor oldMetaAcc;
    if (get(oldMetaAcc, oldUuid))
        remapFile(oldMetaAcc, newUuidAcc, oldUuid, newUuid, newPath);
}

void MetadataCache::map(Path path, std::string uuid)
{
    UuidAccessor uuidAcc;
    m_pathToUuid.insert(uuidAcc, path);

    MetaAccessor metaAcc;
    m_metaCache.insert(metaAcc, uuid);

    uuidAcc->second = std::move(uuid);
    metaAcc->second.path = std::move(path);
}

void MetadataCache::map(
    Path path, FileLocation location, const one::helpers::FlagsSet flags)
{
    auto filteredFlags = filterFlagsForLocation(flags);

    UuidAccessor uuidAcc;
    m_pathToUuid.insert(uuidAcc, path);

    MetaAccessor metaAcc;
    m_metaCache.insert(metaAcc, location.uuid());

    uuidAcc->second = location.uuid();
    metaAcc->second.path = std::move(path);
    metaAcc->second.locations[filteredFlags] = std::move(location);
}

void MetadataCache::remove(UuidAccessor &uuidAcc, MetaAccessor &metaAcc)
{
    m_metaCache.erase(metaAcc);
    m_pathToUuid.erase(uuidAcc);
}

void MetadataCache::removePathMapping(
    UuidAccessor &uuidAcc, MetaAccessor &metaAcc)
{
    m_pathToUuid.erase(uuidAcc);
}

void MetadataCache::remove(const std::string &uuid)
{
    MetaAccessor metaAcc;
    if (!m_metaCache.find(metaAcc, uuid))
        return;

    if (metaAcc->second.path) {
        UuidAccessor uuidAcc;
        if (m_pathToUuid.find(uuidAcc, metaAcc->second.path.get())) {
            remove(uuidAcc, metaAcc);
            return;
        }
    }

    m_metaCache.erase(metaAcc);
}

std::size_t MetadataCache::PathHash::hash(const Path &path)
{
    return std::hash<std::string>{}(path.string());
}

bool MetadataCache::PathHash::equal(const Path &a, const Path &b)
{
    return a == b;
}

bool MetadataCache::waitForNewLocation(const std::string &uuid,
    const boost::icl::discrete_interval<off_t> &range,
    const std::chrono::milliseconds &timeout,
    const one::helpers::FlagsSet flags)
{
    LOG(INFO) << "Waiting for file_location of '" << uuid << "' at range "
              << range;
    auto pair = getMutexConditionPair(uuid);
    std::unique_lock<std::mutex> lock{pair.first};

    const auto pred = [&] {
        FileLocation location = getLocation(uuid, flags);
        return location.blocks().find(boost::icl::first(range)) !=
            location.blocks().end();
    };

    for (auto t = 0ms; t < timeout; ++t) {
        if (helpers::fuseInterrupted())
            throw std::system_error{
                std::make_error_code(std::errc::operation_canceled)};

        if (pair.second.wait_for(lock, 1ms, pred))
            return true;
    }

    return false;
}

void MetadataCache::notifyNewLocationArrived(const std::string &uuid)
{
    MutexAccessor acc;
    if (m_mutexConditionPairMap.find(acc, uuid)) {
        std::condition_variable &condition = *acc->second.second;
        condition.notify_all();
    }
}

std::pair<std::mutex &, std::condition_variable &>
MetadataCache::getMutexConditionPair(const std::string &uuid)
{
    MutexAccessor acc;
    if (m_mutexConditionPairMap.insert(acc, uuid))
        acc->second = std::make_pair(std::make_unique<std::mutex>(),
            std::make_unique<std::condition_variable>());
    return {*acc->second.first, *acc->second.second};
}

MetadataCache::FileAttr MetadataCache::fetchAttr(
    messages::fuse::GetFileAttr request)
{
    auto future = m_communicator.communicate<FileAttr>(std::move(request));

    auto attr = communication::wait(future);
    if (!attr.size().is_initialized())
        throw std::errc::protocol_error;

    return attr;
}

} // namespace one
} // namespace client
