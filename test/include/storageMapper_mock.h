/**
 * @file storageMapper_mock.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_MAPPER_MOCK_H
#define STORAGE_MAPPER_MOCK_H

#include "storageMapper.h"
#include "testCommon.h"

#include "config.h"

#include <memory>

using namespace veil::protocol::fuse_messages;
using namespace veil::protocol::communication_protocol;

class MockStorageMapper
    : public StorageMapper {

public:
    MockStorageMapper(std::shared_ptr<Context> context,
                      std::shared_ptr<FslogicProxy> fslogicProxy)
        : StorageMapper(std::move(context), fslogicProxy) {}

    ~MockStorageMapper() {}

    MOCK_METHOD1(releaseFile, void(const string&));
    MOCK_METHOD2(getLocationInfo, pair<locationInfo, storageInfo>(const string&, bool));
    MOCK_METHOD2(addLocation, void(const string&, const FileLocation&));
    MOCK_METHOD1(findLocation, string(const string&));
};
#endif // STORAGE_MAPPER_MOCK_H
