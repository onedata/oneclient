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

/**
 * @c RemoteSubscriptionHandle class manages the lifetime of a remote
 * subscription and is responsible on creation for sending a subscription
 * message to the remote producer and on deletion for sending cancellation
 * message.
 */
class RemoteSubscriptionHandle : public SubscriptionHandle {
public:
    /**
     * Constructor. Sends subscription message to the remote producer.
     * @param streamKey A key of a stream associated with the handle by a
     * subscription.
     * @param streams A collection of existing event streams.
     * @param subscriptionId An ID of a subscription associated with the handle.
     * @param msg A serialized subscription message that will be sent to the
     * remote producer.
     * @param stream A @c SequencerStream instance.
     */
    RemoteSubscriptionHandle(StreamKey streamKey, Streams &streams,
        std::int64_t subscriptionId, ProtoSubscriptionPtr msg,
        SequencerStream &stream);

    /**
     * Sends subscription cancellation to the remote producer.
     */
    virtual ~RemoteSubscriptionHandle();

private:
    std::int64_t m_subscriptionId;
    SequencerStream &m_stream;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_REMOTE_SUBSCRIPTION_HANDLE_H
