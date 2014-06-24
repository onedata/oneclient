/**
 * @file storageMapper_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_MAPPER_PROXY_H
#define STORAGE_MAPPER_PROXY_H


#include "storageMapper.h"

class ProxyStorageMapper: public veil::client::StorageMapper
{
public:
    ProxyStorageMapper(std::shared_ptr<Context> context,
                       boost::shared_ptr<FslogicProxy> mock)
        : StorageMapper(std::move(context), mock)
    {
    }

    std::map<int, storageInfo>& getStorageMapping()
    {
        return m_storageMapping;
    }

    std::map<std::string, locationInfo>& getFileMapping()
    {
        return m_fileMapping;
    }
};


#endif // STORAGE_MAPPER_PROXY_H
