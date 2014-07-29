/**
 * @file metaCache_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef META_CACHE_PROXY_H
#define META_CACHE_PROXY_H


#include "metaCache.h"

class ProxyMetaCache: public veil::client::MetaCache {
public:
    ProxyMetaCache(std::shared_ptr<veil::client::Context> context)
        : MetaCache{std::move(context)}
    {
    }

    bool canUseDefaultPermissions(const struct stat &attrs) override
    {
        return true;
    }

    auto getStatMap() -> decltype(m_statMap)&
    {
        return m_statMap;
    }
};


#endif // META_CACHE_PROXY_H
