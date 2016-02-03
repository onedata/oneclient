/**
 * @file metadataCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "metadataCache.h"

#include "messages/fuse/getFileAttr.h"
#include "messages/fuse/getFileLocation.h"
#include "messages/fuse/rename.h"

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

std::mutex &MetadataCache::getMutex(const std::string &uuid)
{
    MutexAccessor acc;
    if (m_mutexMap.insert(acc, uuid))
        ;
    acc->second = std::make_unique<std::mutex>();
    return *acc->second;
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
        auto future = m_communicator.communicate<FileAttr>(
            messages::fuse::GetFileAttr{path});

        auto attr = communication::wait(future);
        m_metaCache.insert(metaAcc, attr.uuid());
        metaAcc->second.attr = attr;
        metaAcc->second.path = path;
        uuidAcc->second = attr.uuid();
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
    if (!m_metaCache.insert(metaAcc, uuid))
        if (metaAcc->second.attr)
            return;

    try {
        auto future = m_communicator.communicate<FileAttr>(
            messages::fuse::GetFileAttr{uuid});

        metaAcc->second.attr = communication::wait(future);
    }
    catch (...) {
        m_metaCache.erase(metaAcc);
        throw;
    }
}

void MetadataCache::getLocation(
    MetadataCache::MetaAccessor &metaAcc, const std::string &uuid)
{
    if (!m_metaCache.insert(metaAcc, uuid))
        if (metaAcc->second.location)
            return;

    try {
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

        auto future = m_communicator.communicate<messages::fuse::FuseResponse>(
            messages::fuse::Rename{uuid, newPath});

        communication::wait(future);

        metaAcc->second.path = newPath;
        newUuidAcc->second = uuid;
        m_pathToUuid.erase(oldUuidAcc);
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
    metaAcc->second.path = std::move(path);
}

void MetadataCache::map(Path path, FileLocation location)
{
    UuidAccessor uuidAcc;
    m_pathToUuid.insert(uuidAcc, path);

    MetaAccessor metaAcc;
    m_metaCache.insert(metaAcc, location.uuid());

    uuidAcc->second = location.uuid();
    metaAcc->second.path = std::move(path);
    metaAcc->second.location = std::move(location);
}

void MetadataCache::remove(UuidAccessor &uuidAcc, MetaAccessor &metaAcc)
{
    m_metaCache.erase(metaAcc);
    m_pathToUuid.erase(uuidAcc);
}

std::size_t MetadataCache::PathHash::hash(const Path &path)
{
    return std::hash<std::string>{}(path.string());
}

bool MetadataCache::PathHash::equal(const Path &a, const Path &b)
{
    return a == b;
}

bool MetadataCache::syncAndWaitForNewLocation(const std::string &uuid,
    const boost::icl::discrete_interval<off_t> &range,
    std::unique_lock<std::mutex> &lock,
    const std::chrono::milliseconds &timeout)
{
    LOG(INFO) << "Waiting for file_location of '" << uuid << "' at range "
              << range;

    const auto pred = [&] {
        FileLocation location = getLocation(uuid);
        return location.blocks().find(boost::icl::first(range)) !=
            location.blocks().end();
    };

    return m_newLocationCondition.wait_for(lock, timeout, pred);
}

void MetadataCache::notifyNewLocationArrived()
{
    m_newLocationCondition.notify_all();
}

} // namespace one
} // namespace client
