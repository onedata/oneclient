/**
 * @file localStorageManager_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef LOCAL_STORAGE_MANAGER_MOCK_H
#define LOCAL_STORAGE_MANAGER_MOCK_H


#include "localStorageManager.h"

#include <gmock/gmock.h>

class MockLocalStorageManager: public veil::client::LocalStorageManager
{
public:
    MockLocalStorageManager(std::shared_ptr<veil::client::Context> context)
        : LocalStorageManager{std::move(context)}
    {
    }
};


#endif // LOCAL_STORAGE_MANAGER_MOCK_H
