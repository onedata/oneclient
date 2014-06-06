/**
 * @file localStorageManager_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2014 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in 'LICENSE.txt'
 */

#ifndef LOCAL_STORAGE_MANAGER_MOCK_H
#define LOCAL_STORAGE_MANAGER_MOCK_H

#include "localStorageManager.h"
#include "testCommon.h"

#include "context.h"

#include <memory>

class MockLocalStorageManager
    : public LocalStorageManager {

public:
    MockLocalStorageManager(std::shared_ptr<Context> context)
    	: LocalStorageManager{std::move(context)} {};
    ~MockLocalStorageManager() {};
};
#endif // LOCAL_STORAGE_MANAGER_MOCK_H
