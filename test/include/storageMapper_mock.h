/**
 * @file storageMapper_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_MAPPER_MOCK_H
#define STORAGE_MAPPER_MOCK_H


#include "storageMapper.h"

#include <gmock/gmock.h>

class MockStorageMapper: public veil::client::StorageMapper
{
public:
    MockStorageMapper(std::weak_ptr<veil::client::Context> context,
                      std::shared_ptr<veil::client::FslogicProxy> fslogicProxy)
        : StorageMapper(std::move(context), fslogicProxy)
    {
    }

    MOCK_METHOD1(releaseFile, void(const std::string&));
    MOCK_METHOD3(getLocationInfo, std::pair<veil::client::locationInfo, veil::client::storageInfo>(const std::string&, bool, bool));
    MOCK_METHOD2(addLocation, void(const std::string&, const veil::protocol::fuse_messages::FileLocation&));
    MOCK_METHOD3(findLocation, std::string(const std::string&, const std::string&, bool));
    MOCK_METHOD2(helperOverride, void(const std::string&, const veil::client::storageInfo&));
    MOCK_METHOD1(resetHelperOverride, void(const std::string&));
};


#endif // STORAGE_MAPPER_MOCK_H
