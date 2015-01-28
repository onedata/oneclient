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

/**
* Name of an @c SubscriptionCancellation message.
*/
static const std::string SUBSCRIPTION_CANCELLATION_MESSAGE =
    one::clproto::events::SubscriptionCancellation::descriptor()->name();

/**
* The SubscriptionCancellation class represents a message sent by the server to
* cancel given subscription for events.
*/
class SubscriptionCancellation {
    friend std::ostream &
    operator<<(std::ostream &, const SubscriptionCancellation &subscription);

public:
    /**
    * Constructor.
    * @param id Subscription id.
    */
    SubscriptionCancellation(unsigned long long id);

    /**
    * Processes an subscription cancellation message by removing event
    * subscription from an event stream.
    * @param manager An @c EventManager instance.
    * @param factory An @c EventFactory instance.
    * @param buffer An @c EventBuffer.
    */
    void process(EventManager &manager, std::weak_ptr<EventFactory> factory,
                 std::weak_ptr<EventBuffer> buffer) const;

private:
    unsigned long long m_id;
};

/**
* The SubscriptionCancellationSerializer class is responsible for
* deserialization of the @c SubscriptionCancellation messages.
*/
class SubscriptionCancellationSerializer {
    using Message = one::clproto::communication_protocol::Answer;

public:
public:
    /**
    * Deserializes the @c SubscriptionCancellation message.
    * @param message Message to deserialize.
    * @return Returns deserialized @c SubscriptionCancellation instance.
    */
    std::unique_ptr<SubscriptionCancellation>
    deserialize(const Message &message) const;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_MESSAGES_SUBSCRIPTION_CANCELLATION_H