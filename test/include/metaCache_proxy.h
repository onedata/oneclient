/**
 * @file metaCache_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef META_CACHE_PROXY_H
#define META_CACHE_PROXY_H

#include "metaCache.h"

#include "context.h"
#include "testCommon.h"

#include <memory>

class ProxyMetaCache 
    : public MetaCache {

public:
    ProxyMetaCache(std::shared_ptr<Context> context) : MetaCache{std::move(context)} {}

    std::unordered_map<string, pair<time_t, struct stat> >& getStatMap() {
        return m_statMap;
    }

};


#endif // META_CACHE_PROXY_H
