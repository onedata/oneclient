/**
 * @file fuseFileHandle.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fuseFileHandle.h"

#include "cache/forceProxyIOCache.h"
#include "cache/helpersCache.h"
#include "logging.h"

namespace one {
namespace client {
namespace fslogic {

FuseFileHandle::FuseFileHandle(const int flags_, folly::fbstring handleId,
    std::shared_ptr<cache::LRUMetadataCache::OpenFileToken> openFileToken,
    cache::HelpersCache &helpersCache,
    cache::ForceProxyIOCache &forceProxyIOCache,
    const std::chrono::seconds providerTimeout,
    const unsigned int prefetchCalculateSkipReads,
    const unsigned int prefetchCalculateAfterSeconds)
    : m_flags{flags_}
    , m_handleId{std::move(handleId)}
    , m_openFileToken{std::move(openFileToken)}
    , m_helpersCache{helpersCache}
    , m_forceProxyIOCache{forceProxyIOCache}
    , m_providerTimeout{std::move(providerTimeout)}
    , m_fullPrefetchTriggered{false}
    , m_tagOnCreateSet{false}
    , m_tagOnModifySet{false}
    , m_recentPrefetchOffsets{folly::EvictingCacheMap<off_t, bool>(1000, 50)}
    , m_prefetchCalculateSkipReads{prefetchCalculateSkipReads}
    , m_prefetchCalculateAfterSeconds{prefetchCalculateAfterSeconds}
    , m_readsSinceLastPrefetchCalculation{0}
    , m_timeOfLastPrefetchCalculation{std::chrono::system_clock::now()}
{
}

helpers::FileHandlePtr FuseFileHandle::getHelperHandle(
    const folly::fbstring &uuid, const folly::fbstring &spaceId,
    const folly::fbstring &storageId, const folly::fbstring &fileId)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(storageId) << LOG_FARG(fileId);

    const bool forceProxyIO = m_forceProxyIOCache.contains(uuid);
    const auto key = std::make_tuple(storageId, fileId, forceProxyIO);

    auto it = m_helperHandles.find(key);
    if (it != m_helperHandles.end())
        return it->second;

    auto helper =
        m_helpersCache.get(uuid, spaceId, storageId, forceProxyIO).get();

    if (!helper) {
        LOG(ERROR) << "Could not create storage helper for file " << uuid
                   << " on storage " << storageId;
        throw std::errc::resource_unavailable_try_again;
    }

    const auto filteredFlags = m_flags & (~O_CREAT) & (~O_APPEND);

    auto handle = communication::wait(
        helper->open(fileId, filteredFlags, makeParameters(uuid)),
        m_providerTimeout);

    m_helperHandles[key] = handle;
    return handle;
}

void FuseFileHandle::releaseHelperHandle(const folly::fbstring &uuid,
    const folly::fbstring &storageId, const folly::fbstring &fileId)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(storageId) << LOG_FARG(fileId);

    for (bool forceProxyIO : {true, false}) {
        const auto key = std::make_tuple(storageId, fileId, forceProxyIO);
        auto it = m_helperHandles.find(key);
        if (it != m_helperHandles.end()) {
            communication::wait(it->second->release(), m_providerTimeout);
            m_helperHandles.erase(key);
        }
    }
}

folly::fbvector<helpers::FileHandlePtr> FuseFileHandle::helperHandles() const
{
    folly::fbvector<helpers::FileHandlePtr> result;
    for (auto &elem : m_helperHandles)
        result.emplace_back(elem.second);
    return result;
}

folly::Optional<folly::fbstring> FuseFileHandle::providerHandleId() const
{
    return m_handleId;
}

std::unordered_map<folly::fbstring, folly::fbstring>
FuseFileHandle::makeParameters(const folly::fbstring &uuid)
{
    std::unordered_map<folly::fbstring, folly::fbstring> parameters{
        {"file_uuid", uuid}, {"handle_id", m_handleId}};

    return parameters;
};

bool FuseFileHandle::prefetchAlreadyRequestedAt(off_t offset) const
{
    return m_recentPrefetchOffsets.withRLock(
        [&](const auto &cache) { return cache.exists(offset); });
}

void FuseFileHandle::addPrefetchAt(off_t offset)
{
    return m_recentPrefetchOffsets.withWLock(
        [&](auto &cache) { return cache.set(offset, true); });
}

bool FuseFileHandle::shouldCalculatePrefetch()
{
    if (m_readsSinceLastPrefetchCalculation > m_prefetchCalculateSkipReads ||
        std::chrono::duration_cast<std::chrono::seconds>(
            std::chrono::system_clock::now() - m_timeOfLastPrefetchCalculation)
                .count() > m_prefetchCalculateAfterSeconds) {
        m_readsSinceLastPrefetchCalculation = 0;
        m_timeOfLastPrefetchCalculation = std::chrono::system_clock::now();
        return true;
    }
    else {
        m_readsSinceLastPrefetchCalculation++;
        return false;
    }
}

} // namespace fslogic
} // namespace client
} // namespace one
