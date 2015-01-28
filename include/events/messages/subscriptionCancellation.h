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

namespace clproto {
namespace communication_protocol {
class Answer;
}
}

namespace client {
namespace events {

class EventBuffer;
class EventManager;
class EventFactory;

static const std::string SUBSCRIPTION_CANCELLATION_MESSAGE =
    one::clproto::events::SubscriptionCancellation::descriptor()->name();

class SubscriptionCancellation {
public:
    SubscriptionCancellation(unsigned long long id);

    friend std::ostream &
    operator<<(std::ostream &, const SubscriptionCancellation &subscription);

    void process(EventManager &manager, std::weak_ptr<EventFactory> factory,
                 std::weak_ptr<EventBuffer> buffer) const;

private:
    unsigned long long m_id;
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