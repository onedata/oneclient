/**
 * @file subscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "subscription.h"
#include "subscriptionHandle.h"

namespace one {
namespace client {
namespace events {

SubscriptionHandlePtr Subscription::createHandle(std::int64_t subscriptionId,
    std::int64_t streamId, Router &router, SequencerStream &stream) const
{
    return std::make_unique<SubscriptionHandle>(streamId, routingKey(), router);
}

} // namespace events
} // namespace client
} // namespace one
