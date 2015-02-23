/**
 * @file fsImpl_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef FSIMPL_PROXY_H
#define FSIMPL_PROXY_H


#include "fsImpl.h"

#include <memory>

class ProxyFsImpl: public one::client::FsImpl
{
public:
    ProxyFsImpl(std::string path,
                std::shared_ptr<one::client::Context> context,
                std::shared_ptr<one::client::FslogicProxy> fslogic,
                std::shared_ptr<one::client::MetaCache> metaCache,
                std::shared_ptr<one::client::LocalStorageManager> sManager,
                std::shared_ptr<one::helpers::StorageHelperFactory> sh_factory,
                std::shared_ptr<one::client::events::EventManager> eventManager)
        : FsImpl{path, std::move(context), fslogic, metaCache, sManager, sh_factory, eventManager}
    {
    }

    void setCachedHelper(const std::uint64_t idx,
                         std::shared_ptr<one::helpers::IStorageHelper> sh)
    {
        m_shCache.set(idx, std::move(sh));
    }
};


#endif // FSIMPL_PROXY_H
