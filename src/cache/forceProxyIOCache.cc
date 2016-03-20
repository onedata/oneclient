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

ForceProxyIOCache::ForceProxyIOCache(FsSubscriptions &fsSubscriptions)
    : m_fsSubscriptions{fsSubscriptions}
{
}

bool ForceProxyIOCache::contains(const std::string &fileUuid)
{
    std::shared_lock<std::shared_timed_mutex> lock{m_cacheMutex};
    return m_cache.find(fileUuid) != m_cache.end();
}

void ForceProxyIOCache::insert(const std::string &fileUuid)
{
    {
        std::shared_lock<std::shared_timed_mutex> lock{m_cacheMutex};
        m_cache.insert(fileUuid);
    }
    m_fsSubscriptions.addPermissionChangedSubscription(fileUuid);
}

void ForceProxyIOCache::erase(const std::string &fileUuid)
{
    {
        std::lock_guard<std::shared_timed_mutex> guard{m_cacheMutex};
        m_cache.unsafe_erase(fileUuid);
    }
    m_fsSubscriptions.removePermissionChangedSubscription(fileUuid);
}

} // namespace one
} // namespace client
