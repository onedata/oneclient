/**
 * @file storageMapper_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_MAPPER_PROXY_H
#define STORAGE_MAPPER_PROXY_H


#include "storageMapper.h"

class ProxyStorageMapper: public one::client::StorageMapper
{
public:
    ProxyStorageMapper(std::shared_ptr<one::client::Context> context,
                       std::shared_ptr<one::client::FslogicProxy> mock)
        : StorageMapper(std::move(context), mock)
    {
    }

    std::map<int, one::client::StorageInfo>& getStorageMapping()
    {
        return m_storageMapping;
    }

    std::size_t getFileMappingSize()
    {
        return m_fileMapping.size();
    }
};


#endif // STORAGE_MAPPER_PROXY_H
