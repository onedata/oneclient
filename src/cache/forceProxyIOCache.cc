/**
 * @file forceProxyIOCache.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "forceProxyIOCache.h"

namespace one {
namespace client {
namespace cache {

bool ForceProxyIOCache::contains(const folly::fbstring &fileUuid)
{
    ConstCacheAcc acc;
    return m_cache.find(acc, fileUuid);
}

void ForceProxyIOCache::add(const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    CacheAcc acc;
    if (m_cache.insert(acc, fileUuid)) {
        LOG_DBG(1) << "Adding " << fileUuid << " to ForceProxyIOCache";
        acc->second = true;
        m_onAdd(fileUuid);
    }
}

void ForceProxyIOCache::remove(const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    CacheAcc acc;
    if (m_cache.find(acc, fileUuid)) {
        LOG_DBG(1) << "Invalidating ForceProxyIOCache for uuid: '" << fileUuid
                   << "'";
        m_onRemove(fileUuid);
        m_cache.erase(acc);
    }
}

void ForceProxyIOCache::onAdd(std::function<void(const folly::fbstring &)> cb)
{
    m_onAdd = std::move(cb);
}

void ForceProxyIOCache::onRemove(
    std::function<void(const folly::fbstring &)> cb)
{
    m_onRemove = std::move(cb);
}

} // namespace cache
} // namespace client
} // namespace one
