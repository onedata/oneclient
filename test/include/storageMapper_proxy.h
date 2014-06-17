/**
 * @file storageMapper_proxy.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_MAPPER_PROXY_H
#define STORAGE_MAPPER_PROXY_H

#include "storageMapper.h"
#include "testCommon.h"

#include "context.h"

#include <memory>

class ProxyStorageMapper
    : public StorageMapper {

public:
    ProxyStorageMapper(std::shared_ptr<Context> context,
                       boost::shared_ptr<FslogicProxy> mock)
        : StorageMapper(std::move(context), mock) {}

    map<int, storageInfo>& getStorageMapping() {
        return m_storageMapping;
    }

    map<string, locationInfo>& getFileMapping() {
        return m_fileMapping;
    }

};
#endif // STORAGE_MAPPER_PROXY_H
