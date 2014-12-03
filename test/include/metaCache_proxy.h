/**
 * @file metaCache_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef META_CACHE_PROXY_H
#define META_CACHE_PROXY_H


#include "metaCache.h"

class ProxyMetaCache: public one::client::MetaCache {
public:
    ProxyMetaCache(std::shared_ptr<one::client::Context> context, std::shared_ptr<one::client::FslogicProxy> fslproxy)
        : MetaCache{std::move(context), std::move(fslproxy)}
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
