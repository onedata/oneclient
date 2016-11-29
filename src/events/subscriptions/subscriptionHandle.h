/**
 * @file subscriptionHandle.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_HANDLE_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_HANDLE_H

#include "events/declarations.h"

namespace one {
namespace client {
namespace events {

class SubscriptionHandle {
public:
    SubscriptionHandle(
        std::int64_t streamId, std::string routingKey, Router &router);

    virtual ~SubscriptionHandle();

private:
    std::int64_t m_streamId;
    std::string m_routingKey;
    Router &m_router;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_HANDLE_H
