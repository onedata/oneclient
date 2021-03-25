/**
 * @file readdirCache.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "cache/readdirCache.h"

#include "communication/communicator.h"
#include "fslogic/virtualfs/virtualFsRegistry.h"
#include "helpers/logging.h"
#include "messages/fuse/fileChildrenAttrs.h"
#include "messages/fuse/getFileChildren.h"
#include "messages/fuse/getFileChildrenAttrs.h"
#include "options/options.h"

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
    std::shared_ptr<virtualfs::VirtualFsHelpersCache> virtualFsHelpersCache,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_metadataCache(metadataCache)
    , m_context{std::move(context)}
    , m_virtualFsHelpersCache{std::move(virtualFsHelpersCache)}
    , m_providerTimeout(m_context.lock()->options()->getProviderTimeout())
    , m_prefetchSize(m_context.lock()->options()->getReaddirPrefetchSize())
    , m_runInFiber{std::move(runInFiber)}
{
}

void ReaddirCache::fetch(const folly::fbstring &uuid,
    const bool includeReplicationStatus, const bool includeHardLinkCount)
{
    LOG_FCALL() << LOG_FARG(uuid);

    assertInFiber();

    // This private method is only called from a lock_guard block, which makes
    // sure before that uuid is no longer member of m_cache, so that we don't
    // have to check again here
    auto p = std::make_shared<folly::SharedPromise<folly::Unit>>();
    m_cache.emplace(uuid, p);

    m_context.lock()->scheduler()->post([this, uuid = uuid, p = std::move(p),
                                            includeReplicationStatus,
                                            includeHardLinkCount] {
        std::size_t chunkIndex = 0;
        std::size_t fetchedSize = 0;
        auto isLast = false;

        // Start with empty index token, and then if server returns
        // index token pass to next request.
        folly::Optional<folly::fbstring> indexToken;

        folly::fbvector<folly::Future<folly::Unit>> futs;

        do {
            LOG_DBG(2) << "Requesting directory entries for directory " << uuid
                       << " starting at offset " << chunkIndex;

            auto ew = folly::try_and_catch<std::exception>(
                [this, p, &isLast, uuid, &chunkIndex, &fetchedSize, &futs,
                    &indexToken, includeReplicationStatus,
                    includeHardLinkCount]() {
                    auto msg =
                        communicate<one::messages::fuse::FileChildrenAttrs>(
                            one::messages::fuse::GetFileChildrenAttrs{uuid,
                                static_cast<off_t>(chunkIndex), m_prefetchSize,
                                indexToken, includeReplicationStatus,
                                includeHardLinkCount},
                            m_providerTimeout);

                    fetchedSize = msg.childrenAttrs().size();
                    indexToken.assign(msg.indexToken());
                    isLast = msg.isLast() && *msg.isLast();
                    chunkIndex += fetchedSize;

                    folly::Promise<folly::Unit> partialPromise;
                    futs.emplace_back(partialPromise.getFuture());
                    m_runInFiber([this, msg = std::move(msg),
                                     partialPromise = std::move(partialPromise),
                                     uuid, includeReplicationStatus]() mutable {
                        for (const auto it :
                            folly::enumerate(msg.childrenAttrs())) {
                            auto attr = std::make_shared<FileAttr>(*it);
                            if (includeReplicationStatus &&
                                (attr->type() == FileAttr::FileType::regular) &&
                                !attr->fullyReplicated())
                                continue;

                            m_metadataCache.updateAttr(std::move(attr), true);
                        }
                        partialPromise.setValue();
                    });
                });

            if (bool(ew)) {
                p->setException(ew);
                return folly::Unit();
            }

        } while (!isLast && fetchedSize > 0);

        folly::collectAll(futs)
            .then([this, p, uuid](
                      const std::vector<folly::Try<folly::Unit>> & /*unused*/) {
                // Wait until all directory entries are added to the
                // metadata cache
                m_runInFiber([this, p, uuid]() {
                    m_metadataCache.setDirectorySynced(uuid);
                    p->setValue();
                });
            })
            .get();

        return folly::Unit();
    });
}

folly::fbvector<folly::fbstring> ReaddirCache::readdir(
    const folly::fbstring &uuid, off_t off, std::size_t chunkSize,
    bool includeReplicationStatus, bool includeHardLinkCount)
{
    LOG_FCALL() << LOG_FARG(uuid) << LOG_FARG(off) << LOG_FARG(chunkSize);

    assertInFiber();

    folly::fbstring effectiveUuid{uuid};
    auto virtualStorageId = m_virtualFsHelpersCache->match(uuid);
    auto virtualMode = !virtualStorageId.empty();
    if (virtualMode) {
        effectiveUuid =
            m_virtualFsHelpersCache->get(virtualStorageId)->effectiveName(uuid);
    }

    // Check if the directory is already in the metadata cache, if yes,
    // just return the result
    auto attr = m_metadataCache.getAttr(uuid);
    auto includeVirtualEntries =
        virtualMode || (attr->isVirtual() && !attr->isVirtualEntrypoint());
    if (m_metadataCache.isDirectorySynced(effectiveUuid)) {
        // Clean temporary dir contents fetch cache
        m_cache.erase(effectiveUuid);

        if (includeVirtualEntries) {
            assert(attr->isVirtual());
            attr->getVirtualFsAdapter()->readdir(
                m_metadataCache.readdir(effectiveUuid, off, chunkSize, true,
                    includeReplicationStatus, includeHardLinkCount),
                attr, m_metadataCache);
        }

        return m_metadataCache.readdir(effectiveUuid, off, chunkSize,
            includeVirtualEntries, includeReplicationStatus);
    }

    if (!attr->isVirtual() ||
        attr->getVirtualFsAdapter()->fetchRemoteDirectoryContents(attr)) {
        auto uuidIt = m_cache.find(effectiveUuid);

        if (uuidIt == m_cache.end()) {
            fetch(
                effectiveUuid, includeReplicationStatus, includeHardLinkCount);
        }

        auto dirEntriesFuture = (*m_cache.find(effectiveUuid)).second;
        auto f = dirEntriesFuture->getFuture().wait();

        if (f.hasException()) {
            m_cache.erase(effectiveUuid);
            f.get();
        }
    }

    if (virtualMode || attr->isVirtual()) {
        attr->getVirtualFsAdapter()->readdir(
            m_metadataCache.readdir(effectiveUuid, off, chunkSize,
                includeVirtualEntries, includeReplicationStatus),
            attr, m_metadataCache);

        m_metadataCache.setDirectorySynced(effectiveUuid);
    }

    assert(m_metadataCache.isDirectorySynced(effectiveUuid));

    return m_metadataCache.readdir(effectiveUuid, off, chunkSize,
        includeVirtualEntries, includeReplicationStatus);
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
