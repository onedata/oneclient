/**
 * @file forceProxyIOCache.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "forceProxyIOCache.h"

#include <glog/logging.h>

namespace one {
namespace client {
namespace cache {

bool ForceProxyIOCache::contains(const folly::fbstring &fileUuid)
{
    ConstCacheAcc acc;
    if (m_cache.find(acc, fileUuid)) {
        return true;
    }
    return false;
}

void ForceProxyIOCache::add(const folly::fbstring &fileUuid)
{
    CacheAcc acc;
    if (m_cache.insert(acc, fileUuid)) {
        acc->second = true;
        m_onAdd(fileUuid);
    }
}

void ForceProxyIOCache::remove(const folly::fbstring &fileUuid)
{
    CacheAcc acc;
    if (m_cache.find(acc, fileUuid)) {
        LOG(INFO) << "Invalidating ForceProxyIOCache for uuid: '" << fileUuid
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
