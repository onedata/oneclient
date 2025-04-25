/**
 * @file helpersCache.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helpersCache.h"

#include "buffering/bufferAgent.h"
#include "messages.pb.h"
#include "messages/fuse/createStorageTestFile.h"
#include "messages/fuse/getHelperParams.h"
#include "messages/fuse/helperParams.h"
#include "messages/fuse/storageTestFile.h"
#include "messages/fuse/verifyStorageTestFile.h"

#include <folly/system/ThreadName.h>

#include <algorithm>
#include <chrono>
#include <functional>

namespace one {
namespace client {
namespace cache {

HelpersCacheThreadSafeAdapter::HelpersCacheThreadSafeAdapter(
    std::unique_ptr<HelpersCacheBase> cache)
    : m_cache{std::move(cache)}
{
}

void HelpersCacheThreadSafeAdapter::setCache(
    std::unique_ptr<HelpersCacheBase> cache)
{
    m_cache = std::move(cache);
}

folly::Future<HelpersCacheBase::HelperPtr> HelpersCacheThreadSafeAdapter::get(
    const folly::fbstring &fileUuid, const folly::fbstring &spaceId,
    const folly::fbstring &storageId, bool forceProxyIO, bool proxyFallback)
{
    assert(m_cache);

    std::lock_guard<std::mutex> l{m_cacheMutex};
    return m_cache->get(
        fileUuid, spaceId, storageId, forceProxyIO, proxyFallback);
}

HelpersCacheBase::AccessType HelpersCacheThreadSafeAdapter::getAccessType(
    const folly::fbstring &storageId)
{
    assert(m_cache);

    std::lock_guard<std::mutex> l{m_cacheMutex};
    return m_cache->getAccessType(storageId);
}

folly::Future<folly::Unit>
HelpersCacheThreadSafeAdapter::refreshHelperParameters(
    const folly::fbstring &storageId)
{
    assert(m_cache);

    std::lock_guard<std::mutex> l{m_cacheMutex};
    return m_cache->refreshHelperParameters(storageId);
}

void HelpersCacheThreadSafeAdapter::onHelperCreated(
    std::function<void(folly::fbstring)> callback)
{
    assert(m_cache);

    std::lock_guard<std::mutex> l{m_cacheMutex};
    m_cache->onHelperCreated(std::move(callback));
}
} // namespace cache
} // namespace client
} // namespace one
