/**
 * @file quotaSubscription.h
 * @author Rafal Slota
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_QUOTA_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_QUOTA_SUBSCRIPTION_H

#include "subscription.h"
#include "messages/clientMessage.h"

namespace one {
namespace client {
namespace events {

/**
 * @c QuotaSubscription is a client side subscription and represents
 * a request for quota_exeeded events.
 */
class QuotaSubscription : public Subscription,
                                      public messages::ClientMessage {
public:
    /**
     * Constructor.
     */
    QuotaSubscription();

    std::string toString() const override;

private:
    std::unique_ptr<one::messages::ProtocolClientMessage>
    serializeAndDestroy() override;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_QUOTA_SUBSCRIPTION_H
