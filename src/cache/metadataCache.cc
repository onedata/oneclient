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
        metaAcc->second.attr.get().name(newPath.filename().string());
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
    UuidAccessor acc;
    m_pathToUuid.insert(acc, std::move(path));
    acc->second = std::move(uuid);
}

void MetadataCache::map(Path path, FileLocation location)
{
    UuidAccessor uuidAcc;
    m_pathToUuid.insert(uuidAcc, std::move(path));

    MetaAccessor metaAcc;
    m_metaCache.insert(metaAcc, location.uuid());

    uuidAcc->second = location.uuid();
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

} // namespace one
} // namespace client
