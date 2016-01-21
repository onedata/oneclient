/**
 * @file forceClusterProxyCache.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "forceClusterProxyCache.h"

#include <stdexcept>
#include <string>

namespace one {
namespace client {

ForceClusterProxyCache::ForceClusterProxyCache(FsSubscriptions &fsSubscriptions)
    : m_fsSubscriptions{fsSubscriptions}
{
}

bool ForceClusterProxyCache::contains(const std::string &fileUuid)
{
    return m_cache.find(fileUuid) != m_cache.end();
}

void ForceClusterProxyCache::insert(const std::string &fileUuid)
{
    m_cache.insert(fileUuid);
    m_fsSubscriptions.addPermissionChangedSubscription(fileUuid);
}

void ForceClusterProxyCache::unsafe_erase(const std::string &fileUuid)
{
    m_cache.unsafe_erase(fileUuid);
    m_fsSubscriptions.removePermissionChangedSubscription(fileUuid);
}

} // namespace one
} // namespace client
