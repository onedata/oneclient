/**
 * @file storageHelperFactory_fake.h
 * @author Rafal Slota
 * @copyright (C) 2013 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef STORAGE_HELPER_FACTORY_FAKE_H
#define STORAGE_HELPER_FACTORY_FAKE_H

#include "helpers/storageHelperFactory.h"
#include "genericHelper_mock.h"
#include "testCommon.h"
#include "gmock/gmock.h"

#include "helpers/IStorageHelper.h"

using namespace veil::helpers;

class FakeStorageHelperFactory
    : public StorageHelperFactory {
public:
    FakeStorageHelperFactory() : StorageHelperFactory{nullptr} {}

    boost::shared_ptr<IStorageHelper> presetMock;

    boost::shared_ptr<IStorageHelper> getStorageHelper(const string &sh_name, const IStorageHelper::ArgsMap &args) override {
        if(presetMock)
            return presetMock;
        else
            return boost::shared_ptr<IStorageHelper>(new MockGenericHelper());
    }

};

#endif //STORAGE_HELPER_FACTORY_FAKE_H
