/**
 * @file readdirCache.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/readdirCache.h"

#include "communication/communicator.h"
#include "helpers/logging.h"
#include "options/options.h"
#include "util/uuid.h"

#include "messages/fuse/fileChildrenAttrs.h"
#include "messages/fuse/getFileChildren.h"
#include "messages/fuse/getFileChildrenAttrs.h"
#include <folly/Enumerate.h>
#include <folly/FBString.h>
#include <folly/Optional.h>
#include <folly/Range.h>
#include <fuse/fuse_lowlevel.h>

#include <memory>

namespace one {
namespace client {
namespace cache {

DirCacheEntry::DirCacheEntry(std::chrono::milliseconds cacheValidityPeriod)
    : m_ctime{0}
    , m_atime{0}
    , m_invalid{false}
    , m_cacheValidityPeriod{cacheValidityPeriod}
{
}

DirCacheEntry::DirCacheEntry(const DirCacheEntry &e)
    : m_ctime{e.m_ctime.load()}
    , m_atime{e.m_atime.load()}
    , m_invalid{e.m_invalid.load()}
    , m_dirEntries{e.m_dirEntries}
    , m_cacheValidityPeriod{e.m_cacheValidityPeriod}
{
}

DirCacheEntry::DirCacheEntry(DirCacheEntry &&e) noexcept
    : m_ctime{e.m_ctime.load()}
    , m_atime{e.m_atime.load()}
    , m_invalid{e.m_invalid.load()}
    , m_dirEntries{std::move(e.m_dirEntries)}
    , m_cacheValidityPeriod{e.m_cacheValidityPeriod}
{
}

void DirCacheEntry::addEntry(const folly::fbstring &name)
{
    m_dirEntries.emplace_back(name);
}

void DirCacheEntry::addEntry(folly::fbstring &&name)
{
    m_dirEntries.emplace_back(std::forward<folly::fbstring>(name));
}

const std::list<folly::fbstring> &DirCacheEntry::dirEntries() const
{
    return m_dirEntries;
}

void DirCacheEntry::invalidate() { m_invalid = true; }

bool DirCacheEntry::isValid(bool sinceLastAccess)
{
    if (sinceLastAccess) {
        // Check validity since the last time the cache was accessed
        return !m_invalid &&
            std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::system_clock::now().time_since_epoch()) -
                std::chrono::milliseconds(m_atime) <
            m_cacheValidityPeriod;
    }
    // Check validity since the last time the cache entry was retrieved
    // from server
    return !m_invalid &&
        std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::system_clock::now().time_since_epoch()) -
            std::chrono::milliseconds(m_ctime) <
        2 * m_cacheValidityPeriod;
}

void DirCacheEntry::touch()
{
    m_atime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch())
                  .count();
}

void DirCacheEntry::markCreated()
{
    m_ctime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch())
                  .count();
}

void DirCacheEntry::unique()
{
    m_dirEntries.sort();
    m_dirEntries.unique();
}

ReaddirCache::ReaddirCache(LRUMetadataCache &metadataCache,
    std::weak_ptr<Context> context, folly::fbstring rootUuid,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_metadataCache(metadataCache)
    , m_context{std::move(context)}
    , m_providerTimeout(m_context.lock()->options()->getProviderTimeout())
    , m_prefetchSize(m_context.lock()->options()->getReaddirPrefetchSize())
    , m_rootUuid{std::move(rootUuid)}
    , m_runInFiber{std::move(runInFiber)}
{
    for (const auto &name : m_context.lock()->options()->getSpaceNames()) {
        m_whitelistedSpaceNames.emplace(name);
    }

    for (const auto &id : m_context.lock()->options()->getSpaceIds()) {
        m_whitelistedSpaceIds.emplace(id);
    }
}

void ReaddirCache::fetch(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    // This private method is only called from a lock_guard block, which makes
    // sure before that uuid is no longer member of m_cache, so that we don't
    // have to check again here
    auto p = std::make_shared<
        folly::SharedPromise<std::shared_ptr<DirCacheEntry>>>();
    m_cache.emplace(uuid, p);

    m_context.lock()->scheduler()->post([
        this, uuid = uuid, p = std::move(p)
    ] {
        p->setWith([=] {
            auto cacheEntry =
                std::make_shared<DirCacheEntry>(m_cacheValidityPeriod);
            cacheEntry->addEntry(".");
            cacheEntry->addEntry("..");

            std::size_t chunkIndex = 0;
            std::size_t fetchedSize = 0;
            auto isLast = false;

            // Start with empty index token, and then if server returns
            // index token pass to next request.
            folly::Optional<folly::fbstring> indexToken;

            do {
                LOG_DBG(2) << "Requesting directory entries for directory "
                           << uuid << " starting at offset " << chunkIndex;

                auto msg = communicate<one::messages::fuse::FileChildrenAttrs>(
                    one::messages::fuse::GetFileChildrenAttrs{uuid,
                        static_cast<off_t>(chunkIndex), m_prefetchSize,
                        indexToken},
                    m_providerTimeout);

                fetchedSize = msg.childrenAttrs().size();
                indexToken.assign(msg.indexToken());
                isLast = msg.isLast() && *msg.isLast();

                for (const auto it : folly::enumerate(msg.childrenAttrs())) {
                    cacheEntry->addEntry(it->name());

                    if (uuid == m_rootUuid && !isSpaceWhitelisted(it->name()))
                        continue;

                    m_runInFiber([ this, attr = *it ] {
                        // Update existing or insert new attribute
                        m_metadataCache.updateAttr(attr);
                    });
                }

                chunkIndex = cacheEntry->dirEntries().size() - 2;

            } while (!isLast && fetchedSize > 0);

            cacheEntry->unique();
            cacheEntry->touch();
            cacheEntry->markCreated();

            m_metadataCache.setDirectorySynced(uuid);

            m_context.lock()->scheduler()->schedule(4 * m_cacheValidityPeriod, [
                uuid = uuid, cacheEntry = cacheEntry, self = shared_from_this()
            ]() { self->purgeWorker(uuid, cacheEntry); });

            return cacheEntry;
        });
    });
}

void ReaddirCache::purgeWorker(
    folly::fbstring uuid, std::shared_ptr<DirCacheEntry> entry)
{
    LOG_FCALL() << LOG_FARG(uuid);

    if (!entry->isValid(false)) {
        LOG_DBG(2) << "Purging stale readdir cache entry " << uuid;

        purge(uuid);
    }
    else {
        LOG_DBG(2) << "Readdir cache entry " << uuid
                   << " still valid - scheduling next purge";

        m_context.lock()->scheduler()->schedule(2 * m_cacheValidityPeriod, [
            uuid = std::move(uuid), entry = std::move(entry),
            self = shared_from_this()
        ]() { self->purgeWorker(uuid, entry); });
    }
}

folly::fbvector<folly::fbstring> ReaddirCache::readdir(
    const folly::fbstring &uuid, off_t off, std::size_t chunkSize)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(off) << LOG_FARG(chunkSize);

    // Check if the uuid is already in the cache, if not start fetch
    // asynchronously and add a shared promise to the cache so if any
    // other request for this uuid comes in the meantime it gets queued
    // on that promise
    // In case of error, the promise contains an exception which can
    // be propagated upwards
    {
        std::lock_guard<std::mutex> lock(m_cacheMutex);

        // Check if the directory is already in the metadata cache, if yes, just
        // return the result
        if (m_metadataCache.isDirectorySynced(uuid)) {
            return m_metadataCache.readdir(uuid, off, chunkSize);
        }

        auto uuidIt = m_cache.find(uuid);

        if (uuidIt != m_cache.cend() && (*uuidIt).second->isFulfilled() &&
            ((*uuidIt).second->getFuture().hasException() ||
                !(*uuidIt).second->getFuture().get()->isValid(off != 0))) {
            m_cache.erase(uuidIt);
            uuidIt = m_cache.end();
        }

        if (uuidIt == m_cache.end()) {
            fetch(uuid);
        }
    }

    auto dirEntriesFuture = (*m_cache.find(uuid)).second;
    auto f = dirEntriesFuture->getFuture().wait();

    if (f.hasException()) {
        f.get();
    }

    auto dirCacheEntry = f.value();

    // Update the cache entry so that it doesn't expire before the entire
    // directory is read
    dirCacheEntry->touch();

    folly::fbvector<folly::fbstring> result;
    std::list<folly::fbstring> acc;
    std::list<folly::fbstring>::const_iterator begin;
    std::list<folly::fbstring>::const_iterator end;

    if (off < 0 ||
        static_cast<std::size_t>(off) >= dirCacheEntry->dirEntries().size())
        return result;

    if (uuid == m_rootUuid &&
        (!m_whitelistedSpaceNames.empty() || !m_whitelistedSpaceIds.empty())) {
        // Filter out non-whitelisted spaces
        folly::fbvector<folly::fbstring> whitelistedSpaces;

        for (const auto &spaceName : dirCacheEntry->dirEntries()) {
            if ((spaceName == ".") || (spaceName == ".."))
                continue;

            if (isSpaceWhitelisted(spaceName))
                acc.emplace_back(spaceName);
        }

        begin = acc.cbegin();
        end = acc.cend();
    }
    else {
        begin = dirCacheEntry->dirEntries().cbegin();
        end = dirCacheEntry->dirEntries().cend();
    }

    std::advance(begin, off);

    std::copy_n(begin,
        std::min(
            chunkSize, static_cast<std::size_t>(std::distance(begin, end))),
        std::back_inserter(result));

    return result;
}

bool ReaddirCache::isSpaceWhitelisted(const folly::fbstring &spaceName)
{
    if (m_whitelistedSpaceNames.empty() && m_whitelistedSpaceIds.empty())
        return true;

    folly::fbstring spaceId;
    auto spaceAttrs = m_metadataCache.getAttr(m_rootUuid, spaceName);
    if (spaceAttrs)
        spaceId = util::uuid::uuidToSpaceId(spaceAttrs->uuid());

    bool spaceIsWhitelistedByName = m_whitelistedSpaceNames.find(spaceName) !=
        m_whitelistedSpaceNames.end();

    bool spaceIsWhitelistedById =
        m_whitelistedSpaceIds.find(spaceId) != m_whitelistedSpaceIds.end();

    LOG_DBG(2) << "Space " << spaceName << "(" << spaceId << ") is "
               << spaceIsWhitelistedByName << ":" << spaceIsWhitelistedById;

    return spaceIsWhitelistedByName || spaceIsWhitelistedById;
}

void ReaddirCache::invalidate(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    std::lock_guard<std::mutex> lock(m_cacheMutex);

    auto it = m_cache.find(uuid);
    if (it != m_cache.cend() && (*it).second->isFulfilled()) {
        (*it).second->getFuture().get()->invalidate();
    }
}

void ReaddirCache::purge(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    std::lock_guard<std::mutex> lock(m_cacheMutex);
    m_cache.erase(uuid);
}

bool ReaddirCache::empty()
{
    LOG_FCALL();

    return m_cache.empty();
}

template <typename SrvMsg, typename CliMsg>
SrvMsg ReaddirCache::communicate(
    CliMsg &&msg, const std::chrono::seconds timeout)
{
    return communication::wait(
        m_context.lock()->communicator()->communicate<SrvMsg>(
            std::forward<CliMsg>(msg)),
        timeout);
}
} // namespace cache
} // namespace client
} // namespace one
