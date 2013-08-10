/**
 * @file storageMapper_proxy.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_MAPPER_PROXY_H
#define STORAGE_MAPPER_PROXY_H

#include "storageMapper.hh"

class ProxyStorageMapper
    : public StorageMapper {

public:
    ProxyStorageMapper(FslogicProxy& mock) : StorageMapper(mock) {}

    map<int, storageInfo>& getStorageMapping() {
        return m_storageMapping;
    }

    map<string, locationInfo>& getFileMapping() {
        return m_fileMapping;
    }

};
#endif // STORAGE_MAPPER_PROXY_H