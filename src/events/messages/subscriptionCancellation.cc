/**
* @file subscriptionCancellation.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "logging.h"

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "events/eventBuffer.h"
#include "events/eventFactory.h"
#include "events/eventManager.h"
#include "events/messages/subscriptionCancellation.h"
#include "events/types/subscriptionCancellationEvent.h"

namespace one {
namespace client {
namespace events {

SubscriptionCancellation::SubscriptionCancellation(unsigned long long id)
    : m_id{id}
{
}

std::ostream &operator<<(std::ostream &ostream,
                         const SubscriptionCancellation &cancellation)
{
    return ostream << "type: 'EVENT REQUEST', ID: '" << cancellation.m_id
                   << "'";
}

void SubscriptionCancellation::process(EventManager &manager,
                                       std::weak_ptr<EventFactory> factory,
                                       std::weak_ptr<EventBuffer> buffer) const
{
    LOG(INFO) << "Event manager processing message (" << *this << ").";
    if (manager.cancelSubscription(m_id)) {
        LOG(INFO) << "Subscription with ID: '" << m_id
                  << "' cancelled succssfully.";
        auto event = factory.lock()->createSubscriptionCancellationEvent(m_id);
        LOG(INFO) << "Pushing event (" << *event << ") to the event buffer.";
        buffer.lock()->push(std::move(event));
    }
}

std::unique_ptr<SubscriptionCancellation>
SubscriptionCancellationSerializer::deserialize(const Message &message) const
{
    one::clproto::events::SubscriptionCancellation subscriptionCancellation{};
    if (subscriptionCancellation.ParseFromString(message.worker_answer())) {
        return std::make_unique<one::client::events::SubscriptionCancellation>(
            subscriptionCancellation.id());
    }
    LOG(WARNING) << "Cannot deserialize message of type: '"
                 << message.message_type()
                 << "' with ID: " << message.message_id();
    return nullptr;
}

} // namespace events
} // namespace client
} // namespace one
