/**
 * @file fuseFileHandle.h
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#pragma once

#include "cache/lruMetadataCache.h"
#include "communication/communicator.h"
#include "helpers/storageHelper.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Hash.h>
#include <folly/Optional.h>
#include <folly/futures/Future.h>

#include <unordered_map>

namespace one {
namespace client {

namespace cache {
class HelpersCache;
class ForceProxyIOCache;
} // namespace cache

namespace fslogic {

/**
 * @c FuseFileHandle is responsible for storing information about open files.
 */
class FuseFileHandle {
public:
    /**
     * Constructor.
     * @param flags Open flags mask.
     * @param openFileToken A token that holds the file's metadata in cache.
     * @param helpersCache Cache from which helper objects can be fetched.
     * @param forceProxyIOCache Cache for determining whether ProxyIO is forced
     * for a file.
     */
    FuseFileHandle(const int flags,
        std::shared_ptr<cache::LRUMetadataCache::OpenFileToken> openFileToken,
        cache::HelpersCache &helpersCache,
        cache::ForceProxyIOCache &forceProxyIOCache);

    /**
     * Retrieves a helper handle for an open file.
     * @param uuid Uuid of the file.
     * @param storageId ID of the storage of the file.
     * @param fileId ID of a file on the storage.
     * @returns A new or cached file handle for the location.
     */
    helpers::FileHandlePtr getHelperHandle(const folly::fbstring &uuid,
        const folly::fbstring &storageId, const folly::fbstring &fileId);

    /**
     * Releases an open helper handle for a file.
     * @todo @c getHelperHandle could return a releaseable object instead that
     * would wrap a @c FileHandlePtr .
     * @param uuid Uuid of the file.
     * @param storageId ID of the storage of the file.
     * @param fileId ID of a file on the storage.
     */
    void releaseHelperHandle(const folly::fbstring &uuid,
        const folly::fbstring &storageId, const folly::fbstring &fileId);

    /**
     * @returns Open flags with which the handle was created.
     */
    int flags() const { return m_flags; }

    /**
     * @returns All helper file handles open in this handle.
     */
    folly::fbvector<helpers::FileHandlePtr> helperHandles() const;

    /**
     * @returns A handleID representing the handle on the server.
     */
    folly::Optional<folly::fbstring> providerHandleId() const;

private:
    std::unordered_map<folly::fbstring, folly::fbstring> makeParameters(
        const folly::fbstring &uuid);

    const int m_flags;
    std::shared_ptr<cache::LRUMetadataCache::OpenFileToken> m_openFileToken;
    cache::HelpersCache &m_helpersCache;
    cache::ForceProxyIOCache &m_forceProxyIOCache;
    std::unordered_map<std::tuple<folly::fbstring, folly::fbstring, bool>,
        helpers::FileHandlePtr>
        m_handles;
};

} // namespace fslogic
} // namespace client
} // namespace one
