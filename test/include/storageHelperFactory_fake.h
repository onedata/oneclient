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
#include "helpers/IStorageHelper.h"

#include <memory>

class FakeStorageHelperFactory: public veil::helpers::StorageHelperFactory
{
public:
    FakeStorageHelperFactory()
        : StorageHelperFactory{nullptr, BufferLimits{}}
    {
    }

    std::shared_ptr<IStorageHelper> presetMock;

    std::shared_ptr<IStorageHelper> getStorageHelper(const std::string &sh_name,
                                                     const IStorageHelper::ArgsMap &args) override
    {
        if(presetMock)
            return presetMock;

        return std::make_shared<MockGenericHelper>();
    }
};

#endif //STORAGE_HELPER_FACTORY_FAKE_H
