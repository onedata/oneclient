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


bool ForceClusterProxyCache::contains(const std::string &fileUuid)
{
    return m_cache.find(fileUuid) != m_cache.end();
}

void ForceClusterProxyCache::insert(const std::string &fileUuid)
{
    m_cache.insert(fileUuid);
}

void ForceClusterProxyCache::unsafe_erase(const std::string &fileUuid)
{
    m_cache.unsafe_erase(fileUuid);
}

} // namespace one
} // namespace client
