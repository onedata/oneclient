/**
 * @file subscriptionRegistry_mock.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TEST_UNIT_SUBSCRIPTION_REGISTRY_MOCK_H
#define ONECLIENT_TEST_UNIT_SUBSCRIPTION_REGISTRY_MOCK_H

#include "events/subscriptionRegistry.h"
#include "events/subscriptions/eventSubscriptionCancellation.h"

#include <gmock/gmock.h>

#include <utility>
#include <future>
#include <functional>

namespace one {
namespace client {
class Context;
}
}

class MockSubscriptionRegistry
    : public one::client::events::SubscriptionRegistry {
public:
    MockSubscriptionRegistry(std::shared_ptr<one::client::Context> ctx)
        : SubscriptionRegistry{std::move(ctx)}
    {
    }

    MOCK_METHOD1(
        add, std::future<bool>(std::pair<uint64_t, std::function<void()>>));
    MOCK_METHOD1(remove,
        std::future<bool>(one::client::events::EventSubscriptionCancellation));
};

#endif // ONECLIENT_TEST_UNIT_SUBSCRIPTION_REGISTRY_MOCK_H
