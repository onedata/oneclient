/**
* @file subscriptionCancellation.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "logging.h"
#include "events/eventManager.h"
#include "events/messages/subscriptionCancellation.h"

namespace one {
namespace client {
namespace events {

SubscriptionCancellation::SubscriptionCancellation(std::string id)
    : m_id{std::move(id)}
{
}

bool SubscriptionCancellation::process(EventManager &manager) const
{
    return manager.unregisterEventStream(m_id);
}

std::unique_ptr<EventMessage>
SubscriptionCancellationSerializer::deserialize(const Message &message) const
{
    one::clproto::events::SubscriptionCancellation subscriptionCancellation{};
    if (subscriptionCancellation.ParseFromString(message.worker_answer())) {
        //        return
        //        std::make_unique<one::client::events::SubscriptionCancellation>(
        //            subscriptionCancellation.id());
        return nullptr;
    }
    LOG(WARNING) << "Cannot deserialize message of type: '"
                 << message.message_type()
                 << "' with ID: " << message.message_id();
    return nullptr;
}

} // namespace events
} // namespace client
} // namespace one
