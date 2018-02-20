/**
 * @file readdirCache.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/readdirCache.h"

#include "communication/communicator.h"
#include "logging.h"
#include "options/options.h"

#include "messages/fuse/fileChildrenAttrs.h"
#include "messages/fuse/getFileChildren.h"
#include "messages/fuse/getFileChildrenAttrs.h"
#include <folly/Enumerate.h>
#include <folly/FBString.h>
#include <folly/Optional.h>
#include <folly/Range.h>
#include <fuse/fuse_lowlevel.h>

namespace one {
namespace client {
namespace cache {

using namespace std::literals;

DirCacheEntry::DirCacheEntry(const DirCacheEntry &e)
{
    atime = e.atime.load();
    invalid = e.invalid.load();
    dirEntries = e.dirEntries;
}

DirCacheEntry::DirCacheEntry(DirCacheEntry &&e)
{
    atime = e.atime.load();
    invalid = e.invalid.load();
    dirEntries = std::move(e.dirEntries);
}

const auto &DirCacheEntry::cDirEntries() const { return dirEntries; }

void DirCacheEntry::invalidate() { invalid = true; }

bool DirCacheEntry::isValid(std::chrono::milliseconds duration)
{
    return !invalid &&
        std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::system_clock::now().time_since_epoch()) -
            std::chrono::milliseconds(atime) <
        duration;
}

void DirCacheEntry::touch(void)
{
    atime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch())
                .count();
}

ReaddirCache::ReaddirCache(
    LRUMetadataCache &metadataCache, std::shared_ptr<Context> context)
    : m_metadataCache(metadataCache)
    , m_context{std::move(context)}
    , m_providerTimeout(m_context->options()->getProviderTimeout())
    , m_prefetchSize(m_context->options()->getReaddirPrefetchSize())
{
}

void ReaddirCache::fetch(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    auto p = std::make_shared<folly::SharedPromise<DirCacheEntry>>();
    m_cache.emplace(uuid, p);

    m_context->scheduler()->schedule(0s, [
        this, uuid = uuid, p = std::move(p)
    ] {
        p->setWith([=] {
            DirCacheEntry cacheEntry;
            cacheEntry.dirEntries.emplace_back(".");
            cacheEntry.dirEntries.emplace_back("..");

            std::size_t chunkIndex = 0;
            std::size_t fetchedSize = 0;
            auto isLast = false;

            do {
                // Start with empty index token, and then if server returns
                // index token pass to next request.
                folly::Optional<folly::fbstring> indexToken;

                LOG_DBG(2) << "Requesting directory entries for directory "
                           << uuid << " starting at offset " << chunkIndex;

                auto msg = communicate<one::messages::fuse::FileChildrenAttrs>(
                    one::messages::fuse::GetFileChildrenAttrs{uuid,
                        static_cast<off_t>(chunkIndex), m_prefetchSize,
                        indexToken},
                    m_providerTimeout);

                fetchedSize = msg.childrenAttrs().size();
                indexToken = msg.indexToken();
                isLast = msg.isLast() && *msg.isLast();

                for (const auto it : folly::enumerate(msg.childrenAttrs())) {
                    const auto fileAttrPtr = std::make_shared<FileAttr>(*it);
                    m_metadataCache.putAttr(fileAttrPtr);
                    cacheEntry.dirEntries.emplace_back(fileAttrPtr->name());
                }

                chunkIndex = cacheEntry.dirEntries.size() - 2;

            } while (!isLast && fetchedSize > 0);

            cacheEntry.touch();

            return cacheEntry;
        });
    });
}

folly::fbvector<folly::fbstring> ReaddirCache::readdir(
    const folly::fbstring &uuid, off_t off, std::size_t chunkSize)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(off) << LOG_FARG(chunkSize);

    // Check if the uuid is already in the cache if not start fetch
    // asynchronously and add a shared promise to the cache so if any
    // other request for this uuid comes in the mean time it is queued
    // on that promised
    m_cacheMutex.lock();
    auto uuidIt = m_cache.find(uuid);
    if (uuidIt != m_cache.cend() && (*uuidIt).second->isFulfilled() &&
        !(*uuidIt).second->getFuture().get().isValid(m_cacheValidityPeriod)) {
        m_cache.erase(uuid);
    }
    if (m_cache.find(uuid) == m_cache.cend()) {
        fetch(uuid);
    }
    m_cacheMutex.unlock();

    auto dirEntriesFuture = (*m_cache.find(uuid)).second;
    auto f = dirEntriesFuture->getFuture().wait();

    if (f.hasException()) {
        f.get();
    }

    DirCacheEntry &dirCacheEntry = f.value();

    // Update the cache entry so that it doesn't expire before the entire
    // directory is read
    dirCacheEntry.touch();

    folly::fbvector<folly::fbstring> acc;

    if (off < 0 ||
        static_cast<std::size_t>(off) >= dirCacheEntry.cDirEntries().size())
        return acc;

    auto begin = dirCacheEntry.cDirEntries().cbegin();
    auto end = dirCacheEntry.cDirEntries().cend();

    std::advance(begin, off);

    std::copy_n(begin,
        std::min(
            chunkSize, static_cast<std::size_t>(std::distance(begin, end))),
        std::back_inserter(acc));

    return acc;
}

void ReaddirCache::invalidate(const folly::fbstring &uuid)
{
    m_cacheMutex.lock();

    auto it = m_cache.find(uuid);
    if (it != m_cache.cend() && (*it).second->isFulfilled()) {
        (*it).second->getFuture().get().invalidate();
    }

    m_cacheMutex.unlock();
}

template <typename SrvMsg, typename CliMsg>
SrvMsg ReaddirCache::communicate(
    CliMsg &&msg, const std::chrono::seconds timeout)
{
    return communication::wait(m_context->communicator()->communicate<SrvMsg>(
                                   std::forward<CliMsg>(msg)),
        timeout);
}
}
}
}
