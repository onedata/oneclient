/**
 * @file veilfs_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILFS_PROXY_H
#define VEILFS_PROXY_H

#include "veilfs.h"
#include "testCommon.h"

#include "context.h"

#include <memory>

class ProxyVeilFS
    : public veil::client::VeilFS {
public:
    ProxyVeilFS(std::string path, std::shared_ptr<Context> context,
               std::shared_ptr<FslogicProxy> fslogic,  std::shared_ptr<MetaCache> metaCache,
               std::shared_ptr<LocalStorageManager> sManager, std::shared_ptr<StorageMapper> mapper,
               std::shared_ptr<helpers::StorageHelperFactory> sh_factory,
               std::shared_ptr<EventCommunicator> eventCommunicator)
      : VeilFS(path, std::move(context), fslogic, metaCache, sManager, mapper, sh_factory, eventCommunicator)
    {

    }

    void setCachedHelper(helper_cache_idx_t idx, sh_ptr sh)
    {
        m_shCache[idx] = sh;
    }
};



#endif // VEILFS_PROXY_H
