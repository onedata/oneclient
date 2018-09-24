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

SubscriptionHandlePtr Subscription::createHandle(
    std::int64_t /*subscriptionId*/, Streams &streams,
    SequencerStream & /*stream*/) const
{
    return std::make_unique<SubscriptionHandle>(streamKey(), streams);
}

} // namespace events
} // namespace client
} // namespace one
