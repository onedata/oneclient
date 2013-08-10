/**
 * @file storageMapper_mock.hh
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_MAPPER_MOCK_H
#define STORAGE_MAPPER_MOCK_H

#include "storageMapper.hh"
#include "testCommon.hh"

class MockStorageMapper
    : public StorageMapper {

public:
    MockStorageMapper(FslogicProxy& fslogicProxy) : StorageMapper(fslogicProxy) {}
    ~MockStorageMapper() {}

    MOCK_METHOD1(releaseFile, void(string));
    MOCK_METHOD2(getLocationInfo, pair<locationInfo, storageInfo>(string, bool));
    MOCK_METHOD2(addLocation, void(string, FileLocation));
    MOCK_METHOD1(findLocation, string(string));
};
#endif // STORAGE_MAPPER_MOCK_H