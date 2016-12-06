/**
 * @file remoteSubscriptionHandle.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOTE_SUBSCRIPTION_HANDLE_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOTE_SUBSCRIPTION_HANDLE_H

#include "subscriptionHandle.h"

namespace one {
namespace client {
namespace events {

class RemoteSubscriptionHandle : public SubscriptionHandle {
public:
    RemoteSubscriptionHandle(StreamKey streamKey, Streams &streams,
        std::int64_t subscriptionId, ProtoSubscriptionPtr msg,
        SequencerStream &stream);

    virtual ~RemoteSubscriptionHandle();

private:
    std::int64_t m_subscriptionId;
    SequencerStream &m_stream;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOTE_SUBSCRIPTION_HANDLE_H
