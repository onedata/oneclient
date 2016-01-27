/**
 * @file eventManager_mock.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENTMANAGER_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENTMANAGER_MOCK_H

#include "context.h"
#include "events/eventManager.h"

#include <gmock/gmock.h>

class MockEventManager : public one::client::events::EventManager {
public:
    MockEventManager(std::shared_ptr<one::client::Context> ctx)
        : one::client::events::EventManager{ctx}
    {
    }
};

#endif // ONECLIENT_TEST_UNIT_EVENTMANAGER_MOCK_H
