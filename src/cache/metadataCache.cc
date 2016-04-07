/**
 * @file metadataCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "metadataCache.h"

#include "logging.h"
#include "scheduler.h"
#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/rename.h"

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

MetadataCache::FileLocation MetadataCache::getLocation(const std::string &uuid)
{
    ConstMetaAccessor constAcc;
    if (m_metaCache.find(constAcc, uuid)) {
        if (constAcc->second.location)
            return constAcc->second.location.get();

        constAcc.release();
    }

    MetaAccessor acc;
    getLocation(acc, uuid);
    return acc->second.location.get();
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
        metaAcc->second.paths.emplace(path);
        return;
    }

    try {
        DLOG(INFO) << "Fetching attributes for " << path;
        auto attr = fetchAttr(messages::fuse::GetFileAttr{path});
        m_metaCache.insert(metaAcc, attr.uuid());
        uuidAcc->second = attr.uuid();
        metaAcc->second.attr = std::move(attr);
        metaAcc->second.paths.emplace(path);
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

void MetadataCache::getLocation(
    MetadataCache::MetaAccessor &metaAcc, const std::string &uuid)
{
    if (!m_metaCache.insert(metaAcc, uuid)) {
        if (metaAcc->second.location)
            return;
    }

    try {
        DLOG(INFO) << "Fetching file location for " << uuid;
        auto future = m_communicator.communicate<FileLocation>(
            messages::fuse::GetFileLocation{uuid});

        metaAcc->second.location = communication::wait(future);
    }
    catch (...) {
        m_metaCache.erase(metaAcc);
        throw;
    }
}

void MetadataCache::rename(
    const MetadataCache::Path &oldPath, const MetadataCache::Path &newPath)
{
    // By convention, to avoid deadlocks, always lock on path before metadata
    UuidAccessor newUuidAcc;
    m_pathToUuid.insert(newUuidAcc, newPath);

    try {
        UuidAccessor oldUuidAcc;
        MetaAccessor metaAcc;
        getAttr(oldUuidAcc, metaAcc, oldPath);
        auto &uuid = metaAcc->second.attr.get().uuid();

        DLOG(INFO) << "Renaming file " << uuid << " to " << newPath;
        auto future = m_communicator.communicate<messages::fuse::FuseResponse>(
            messages::fuse::Rename{uuid, newPath});

        communication::wait(future);

        auto paths = metaAcc->second.paths;
        metaAcc->second.paths.clear();
        metaAcc->second.paths.emplace(newPath);

        m_pathToUuid.erase(oldUuidAcc);
        newUuidAcc->second = uuid;

        metaAcc.release();
        for (auto &path : paths)
            m_pathToUuid.erase(path);
    }
    catch (...) {
        m_pathToUuid.erase(newUuidAcc);
        throw;
    }
}

void MetadataCache::map(Path path, std::string uuid)
{
    UuidAccessor uuidAcc;
    m_pathToUuid.insert(uuidAcc, path);

    MetaAccessor metaAcc;
    m_metaCache.insert(metaAcc, uuid);

    uuidAcc->second = std::move(uuid);
    metaAcc->second.paths.emplace(std::move(path));
}

void MetadataCache::map(Path path, FileLocation location)
{
    UuidAccessor uuidAcc;
    m_pathToUuid.insert(uuidAcc, path);

    MetaAccessor metaAcc;
    m_metaCache.insert(metaAcc, location.uuid());

    uuidAcc->second = location.uuid();
    metaAcc->second.paths.emplace(std::move(path));
    metaAcc->second.location = std::move(location);
}

void MetadataCache::remove(UuidAccessor &uuidAcc, MetaAccessor &metaAcc)
{
    auto paths = metaAcc->second.paths;
    m_metaCache.erase(metaAcc);
    m_pathToUuid.erase(uuidAcc);

    for (auto &path : paths)
        m_pathToUuid.erase(path);
}

void MetadataCache::removePathMappings(
    UuidAccessor &uuidAcc, MetaAccessor &metaAcc)
{
    metaAcc->second.paths.erase(uuidAcc->first);
    m_pathToUuid.erase(uuidAcc);

    auto paths = metaAcc->second.paths;
    metaAcc->second.paths.clear();
    metaAcc.release();

    for (auto &path : paths)
        m_pathToUuid.erase(path);
}

void MetadataCache::remove(const std::string &uuid)
{
    MetaAccessor metaAcc;
    if (!m_metaCache.find(metaAcc, uuid))
        return;

    auto paths = metaAcc->second.paths;
    m_metaCache.erase(metaAcc);

    for (auto &path : paths)
        m_pathToUuid.erase(path);
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
    const std::chrono::milliseconds &timeout)
{
    LOG(INFO) << "Waiting for file_location of '" << uuid << "' at range "
              << range;
    auto pair = getMutexConditionPair(uuid);
    std::unique_lock<std::mutex> lock{pair.first};

    const auto pred = [&] {
        FileLocation location = getLocation(uuid);
        return location.blocks().find(boost::icl::first(range)) !=
            location.blocks().end();
    };

    return pair.second.wait_for(lock, timeout, pred);
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
