/**
* @file subscriptionCancellationEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "events.pb.h"

#include "events/eventBuffer.h"
#include "events/types/subscriptionCancellationEvent.h"

namespace one {
namespace client {
namespace events {

SubscriptionCancellationEvent::SubscriptionCancellationEvent(
    unsigned long long id)
    : m_id{id}
{
}

void SubscriptionCancellationEvent::emit() {}

std::unique_ptr<EventSerializer>
SubscriptionCancellationEvent::serializer() const
{
    return std::make_unique<SubscriptionCancellationEventSerializer>();
}

std::unique_ptr<google::protobuf::Message>
SubscriptionCancellationEventSerializer::serialize(
    unsigned long long sequenceNumber, const Event &event) const
{
    auto subscriptionCancellationEvent =
        static_cast<const SubscriptionCancellationEvent &>(event);
    auto message =
        std::make_unique<one::clproto::events::SubscriptionCancellationEvent>();
    message->set_seq_num(sequenceNumber);
    message->set_id(subscriptionCancellationEvent.m_id);
    return std::move(message);
}

} // namespace events
} // namespace client
} // namespace one
