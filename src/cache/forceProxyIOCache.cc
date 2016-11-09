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

ForceProxyIOCache::ForceProxyIOCache(FsSubscriptions &fsSubscriptions)
    : m_fsSubscriptions{fsSubscriptions}
{
}

bool ForceProxyIOCache::contains(const folly::fbstring &fileUuid)
{
    return m_cache.count(fileUuid);
}

void ForceProxyIOCache::insert(folly::fbstring fileUuid)
{
    m_cache.emplace(fileUuid);
    m_fsSubscriptions.addPermissionChangedSubscription(fileUuid.toStdString());
}

void ForceProxyIOCache::erase(const folly::fbstring &fileUuid)
{
    m_cache.erase(fileUuid);
    m_fsSubscriptions.removePermissionChangedSubscription(
        fileUuid.toStdString());
}

void ForceProxyIOCache::handlePermissionChanged(const folly::fbstring &fileUuid)
{
    LOG(INFO) << "Invalidating ForceProxyIOCache for uuid: '" << fileUuid
              << "'";

    erase(fileUuid);
}

} // namespace cache
} // namespace client
} // namespace one
