/**
* @file subscriptionCancellation.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_MESSAGES_SUBSCRIPTION_CANCELLATION_H
#define ONECLIENT_EVENTS_MESSAGES_SUBSCRIPTION_CANCELLATION_H

#include "events.pb.h"
#include "eventMessage.h"

namespace one {
namespace client {
namespace events {

static const std::string SUBSCRIPTION_CANCELLATION_MESSAGE =
    one::clproto::events::SubscriptionCancellation::descriptor()->name();

class SubscriptionCancellation : public EventMessage {
public:
    SubscriptionCancellation(std::string id);

    virtual ~SubscriptionCancellation() = default;

    bool process(EventManager &manager) const override;

private:
    std::string m_id;
};

class SubscriptionCancellationSerializer : public EventMessageSerializer {
public:
    virtual ~SubscriptionCancellationSerializer() = default;

    virtual std::unique_ptr<EventMessage>
    deserialize(const Message &message) const override;
};

} // namespace events
} // namespace client
} // namespace one

#endif