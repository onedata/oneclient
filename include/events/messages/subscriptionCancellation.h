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

#include <memory>

namespace one {
namespace client {
namespace events {

class EventManager;

static const std::string SUBSCRIPTION_CANCELLATION_MESSAGE =
    one::clproto::events::SubscriptionCancellation::descriptor()->name();

class SubscriptionCancellation {
public:
    SubscriptionCancellation(std::string id);

    void process(EventManager &manager) const;

private:
    std::string m_id;
};

class SubscriptionCancellationSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
    std::unique_ptr<SubscriptionCancellation>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif