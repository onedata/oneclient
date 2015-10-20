/**
 * @file eventSubscriptionRegistry_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_SUBSCRIPTION_REGISTRY_MOCK_H
#define ONECLIENT_TEST_UNIT_SUBSCRIPTION_REGISTRY_MOCK_H

#include "events/subscriptionRegistry.h"
#include "events/subscriptions/subscriptionCancellation.h"

#include <gmock/gmock.h>

class MockSubscriptionRegistry
    : public one::client::events::SubscriptionRegistry {
public:
    using Subscription = one::client::events::SubscriptionCancellation;

    MOCK_METHOD1(removeSubscription, bool(Subscription));
};

#endif // ONECLIENT_TEST_UNIT_SUBSCRIPTION_REGISTRY_MOCK_H
