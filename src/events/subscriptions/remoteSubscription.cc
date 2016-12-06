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
    std::int64_t subscriptionId, Streams &streams,
    SequencerStream &stream) const
{
    return std::make_unique<RemoteSubscriptionHandle>(
        streamKey(), streams, subscriptionId, serialize(), stream);
}

} // namespace events
} // namespace client
} // namespace one
