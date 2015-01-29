/**
* @file subscriptionEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "events.pb.h"

#include "events/eventBuffer.h"
#include "events/types/subscriptionEvent.h"

namespace one {
namespace client {
namespace events {

SubscriptionEvent::SubscriptionEvent(unsigned long long id)
    : m_id{id}
{
}

std::ostream &operator<<(std::ostream &ostream, const SubscriptionEvent &event)
{
    return ostream << "type: 'SUBSCRIPTION', counter: '" << event.m_counter
                   << "', subscription ID: '" << event.m_id << "'";
}

void SubscriptionEvent::emit() {}

std::unique_ptr<EventSerializer> SubscriptionEvent::serializer() const
{
    return std::make_unique<SubscriptionEventSerializer>();
}

std::unique_ptr<google::protobuf::Message>
SubscriptionEventSerializer::serialize(unsigned long long seqNum,
                                       const Event &event) const
{
    auto subscriptionEvent = static_cast<const SubscriptionEvent &>(event);
    auto message = std::make_unique<one::clproto::events::SubscriptionEvent>();
    message->set_seq_num(seqNum);
    message->set_id(subscriptionEvent.m_id);
    return std::move(message);
}

} // namespace events
} // namespace client
} // namespace one
