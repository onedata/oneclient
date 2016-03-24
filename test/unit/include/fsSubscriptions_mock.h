/**
 * @file fsSubscriptions_mock.h
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_FSSUBSCRIPTIONS_MOCK_H
#define ONECLIENT_TEST_UNIT_FSSUBSCRIPTIONS_MOCK_H

#include "fsSubscriptions.h"

#include <gmock/gmock.h>

class MockFsSubscriptions : public one::client::FsSubscriptions {
public:
    MockFsSubscriptions(one::client::events::EventManager &eventManager)
        : one::client::FsSubscriptions{eventManager}
    {
    }

    MOCK_METHOD1(addPermissionChangedSubscription, void(const std::string &));
    MOCK_METHOD1(
        removePermissionChangedSubscription, void(const std::string &));
};

#endif // ONECLIENT_TEST_UNIT_FSSUBSCRIPTIONS_MOCK_H
