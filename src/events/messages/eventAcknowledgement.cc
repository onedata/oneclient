/**
* @file eventAcknowledgement.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "logging.h"

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "events/eventBuffer.h"
#include "events/messages/eventAcknowledgement.h"

namespace one {
namespace client {
namespace events {

EventAcknowledgement::EventAcknowledgement(unsigned long long id)
    : m_id{id}
{
}

void EventAcknowledgement::process(std::weak_ptr<EventBuffer> buffer) const
{
    buffer.lock()->removeSentMessages(m_id);
}

std::unique_ptr<EventAcknowledgement>
EventAcknowledgementSerializer::deserialize(const Message &message) const
{
    one::clproto::events::EventAcknowledgement eventAcknowledgement{};
    if (eventAcknowledgement.ParseFromString(message.worker_answer())) {
        return std::make_unique<one::client::events::EventAcknowledgement>(
            eventAcknowledgement.seq_num());
    }
    LOG(WARNING) << "Cannot deserialize message of type: '"
                 << message.message_type()
                 << "' with ID: " << message.message_id();
    return nullptr;
}

} // namespace events
} // namespace client
} // namespace one
