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

ReaddirCache::ReaddirCache(OpenFileMetadataCache &metadataCache,
    std::weak_ptr<Context> context,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_metadataCache(metadataCache)
    , m_context{std::move(context)}
    , m_providerTimeout(m_context.lock()->options()->getProviderTimeout())
    , m_prefetchSize(m_context.lock()->options()->getReaddirPrefetchSize())
    , m_runInFiber{std::move(runInFiber)}
{
}

void ReaddirCache::fetch(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    // This private method is only called from a lock_guard block, which makes
    // sure before that uuid is no longer member of m_cache, so that we don't
    // have to check again here
    auto p = std::make_shared<folly::SharedPromise<folly::Unit>>();
    m_cache.emplace(uuid, p);

    std::size_t chunkIndex = 0;
    std::size_t fetchedSize = 0;
    auto isLast = false;

    // Start with empty index token, and then if server returns
    // index token pass to next request.
    folly::Optional<folly::fbstring> indexToken;

    folly::fbvector<folly::Future<folly::Unit>> futs;

    do {
        LOG_DBG(1) << "Requesting directory entries for directory " << uuid
                   << " starting at offset " << chunkIndex;

        auto ew = folly::try_and_catch<std::exception>([this, p, &isLast, uuid,
                                                           &chunkIndex,
                                                           &fetchedSize, &futs,
                                                           &indexToken]() {
            auto msg = communicate<one::messages::fuse::FileChildrenAttrs>(
                one::messages::fuse::GetFileChildrenAttrs{uuid,
                    static_cast<off_t>(chunkIndex), m_prefetchSize, indexToken},
                m_providerTimeout);

            fetchedSize = msg.childrenAttrs().size();
            indexToken.assign(msg.indexToken());
            isLast = msg.isLast() && *msg.isLast();
            chunkIndex += fetchedSize;

            folly::Promise<folly::Unit> partialPromise;
            futs.emplace_back(partialPromise.getFuture());
            m_runInFiber([
                this, msg = std::move(msg),
                partialPromise = std::move(partialPromise), uuid
            ]() mutable {
                for (const auto it : folly::enumerate(msg.childrenAttrs())) {
                    m_metadataCache.updateAttr(std::make_shared<FileAttr>(*it));
                }
                partialPromise.setValue();
            });
        });

        if (bool(ew)) {
            p->setException(ew);
            return;
        }

    } while (!isLast && fetchedSize > 0);

    folly::collectAll(futs)
        .then([this, p, uuid](
                  const std::vector<folly::Try<folly::Unit>> & /*unused*/) {
            // Wait until all directory entries are added to the
            // metadata cache
            m_metadataCache.setDirectorySynced(uuid);
            p->setValue();
        })
        .get();
}

folly::fbvector<folly::fbstring> ReaddirCache::readdir(
    const folly::fbstring &uuid, off_t off, std::size_t chunkSize)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(off) << LOG_FARG(chunkSize);

    assertInFiber();

    // Check if the uuid is already in the cache, if not start fetch
    // asynchronously and add a shared promise to the cache so if any
    // other request for this uuid comes in the meantime it gets queued
    // on that promise
    // In case of error, the promise contains an exception which can
    // be propagated upwards

    // Check if the directory is already in the metadata cache, if yes,
    // just return the result
    if (m_metadataCache.isDirectorySynced(uuid)) {
        m_cache.erase(uuid);
        return m_metadataCache.readdir(uuid, off, chunkSize);
    }

    auto uuidIt = m_cache.find(uuid);

    if (uuidIt == m_cache.end()) {
        fetch(uuid);
    }

    auto dirEntriesFuture = (*m_cache.find(uuid)).second;
    auto f = dirEntriesFuture->getFuture().wait();

    if (f.hasException()) {
        m_cache.erase(uuid);
        f.get();
    }

    assert(m_metadataCache.isDirectorySynced(uuid));

    return m_metadataCache.readdir(uuid, off, chunkSize);
}

void ReaddirCache::purge(const folly::fbstring &uuid)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    m_cache.erase(uuid);
}

bool ReaddirCache::empty()
{
    LOG_FCALL();

    assertInFiber();

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
