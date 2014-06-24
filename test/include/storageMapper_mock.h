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
    MockStorageMapper(std::shared_ptr<Context> context,
                      boost::shared_ptr<FslogicProxy> fslogicProxy)
        : StorageMapper(std::move(context), fslogicProxy)
    {
    }

    MOCK_METHOD1(releaseFile, void(const std::string&));
    MOCK_METHOD2(getLocationInfo, pair<locationInfo, storageInfo>(const std::string&, bool));
    MOCK_METHOD2(addLocation, void(const std::string&, const veil::protocol::fuse_messages::FileLocation&));
    MOCK_METHOD1(findLocation, string(const std::string&));
};


#endif // STORAGE_MAPPER_MOCK_H
