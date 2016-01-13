/**
 * @file subscriptionCancellation.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_CANCELLATION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_CANCELLATION_H

#include "messages/clientMessage.h"
#include "messages/serverMessage.h"

#include "messages.pb.h"

namespace one {
namespace clproto {
class SubscriptionCancellation;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c SubscriptionCancellation class represents event subscription
 * cancellation request sent by the server.
 */
class SubscriptionCancellation : public messages::ClientMessage,
                                 public messages::ServerMessage {
public:
    using ProtocolMessage = clproto::SubscriptionCancellation;

    /**
     * Constructor.
     * @param message Protocol Buffers message representing @c
     * SubscriptionCancellation counterpart.
     */
    SubscriptionCancellation(const ProtocolMessage &message);

    /**
     * Constructor.
     * @param id ID of subscription to be cancelled.
     */
    SubscriptionCancellation(std::int64_t id);

    /**
     * @return ID of subscription to be cancelled.
     */
    std::int64_t id() const;

    std::string toString() const override;

private:
    std::unique_ptr<messages::ProtocolClientMessage>
    serializeAndDestroy() override;

    std::int64_t m_id;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_CANCELLATION_H
