/**
 * @file veilfs_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef VEILFS_PROXY_H
#define VEILFS_PROXY_H


#include "veilfs.h"

#include <memory>

class ProxyVeilFS: public veil::client::VeilFS
{
public:
    ProxyVeilFS(std::string path,
                std::shared_ptr<veil::client::Context> context,
                std::shared_ptr<veil::client::FslogicProxy> fslogic,
                std::shared_ptr<veil::client::MetaCache> metaCache,
                std::shared_ptr<veil::client::LocalStorageManager> sManager,
                std::shared_ptr<veil::client::StorageMapper> mapper,
                std::shared_ptr<veil::helpers::StorageHelperFactory> sh_factory,
                std::shared_ptr<veil::client::events::EventCommunicator> eventCommunicator)
        : VeilFS{path, std::move(context), fslogic, metaCache, sManager, mapper, sh_factory, eventCommunicator}
    {
    }

    void setCachedHelper(veil::client::helper_cache_idx_t idx, veil::client::sh_ptr sh)
    {
        m_shCache[idx] = sh;
    }
};


#endif // VEILFS_PROXY_H
