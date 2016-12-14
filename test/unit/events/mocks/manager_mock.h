/**
 * @file manager_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_EVENTS_MANAGER_MOCK_H
#define ONECLIENT_TEST_UNIT_EVENTS_MANAGER_MOCK_H

#include "../utils.h"
#include "events/manager.h"
#include "events/streams.h"

struct MockManager : public one::client::events::Manager {
    MockManager()
        : MockManager{testContext()}
    {
    }

    MockManager(std::shared_ptr<one::client::Context> context)
        : one::client::events::Manager{std::move(context)}
    {
    }

    MOCK_METHOD1(
        subscribe, std::int64_t(const one::client::events::Subscription &));
    MOCK_METHOD1(unsubscribe, bool(std::int64_t));
    MOCK_METHOD1(flush, void(one::client::events::StreamKey));
};

#endif // ONECLIENT_TEST_UNIT_EVENTS_MANAGER_MOCK_H
