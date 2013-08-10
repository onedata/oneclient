/**
 * @file metaCache_proxy.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef META_CACHE_PROXY_H
#define META_CACHE_PROXY_H

#include "metaCache.hh"

class ProxyMetaCache 
    : public MetaCache {

public:
    map<string, pair<time_t, struct stat> >& getStatMap() {
        return m_statMap;
    }

};


#endif // META_CACHE_PROXY_H