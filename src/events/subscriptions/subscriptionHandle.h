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
    SubscriptionHandle(StreamKey streamKey, Streams &streams);

    virtual ~SubscriptionHandle();

private:
    StreamKey m_streamKey;
    Streams &m_streams;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_HANDLE_H
