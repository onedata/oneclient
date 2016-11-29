/**
 * @file remoteSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "remoteSubscription.h"
#include "remoteSubscriptionHandle.h"

namespace one {
namespace client {
namespace events {

SubscriptionHandlePtr RemoteSubscription::createHandle(
    std::int64_t subscriptionId, std::int64_t streamId, Router &router,
    SequencerStream &stream) const
{
    return std::make_unique<RemoteSubscriptionHandle>(
        streamId, routingKey(), router, subscriptionId, serialize(), stream);
}

} // namespace events
} // namespace client
} // namespace one
