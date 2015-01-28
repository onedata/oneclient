/**
* @file eventRequest.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "logging.h"

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "events/eventBuffer.h"
#include "events/eventCommunicator.h"
#include "events/messages/eventRequest.h"

namespace one {
namespace client {
namespace events {

EventRequest::EventRequest(unsigned long long sequenceNumber)
    : m_sequenceNumber{sequenceNumber}
{
}

std::ostream &operator<<(std::ostream &ostream, const EventRequest &request)
{
    return ostream << "type: 'EVENT REQUEST', sequence number: '"
                   << request.m_sequenceNumber << "'";
}

void EventRequest::process(std::weak_ptr<EventBuffer> buffer,
                           std::weak_ptr<EventCommunicator> communicator) const
{
    LOG(INFO) << "Event manager processing message (" << *this << ").";
    try {
        communicator.lock()->send(
            buffer.lock()->getSentMessage(m_sequenceNumber));
    }
    catch (std::exception &e) {
        LOG(WARNING) << "Cannot send message with sequence number: '"
                     << m_sequenceNumber << "' due to: '" << e.what()
                     << "' exception.";
    }
}

std::unique_ptr<EventRequest>
EventRequestSerializer::deserialize(const Message &message) const
{
    one::clproto::events::EventRequest eventRequest{};
    if (eventRequest.ParseFromString(message.worker_answer())) {
        return std::make_unique<one::client::events::EventRequest>(
            eventRequest.seq_num());
    }
    LOG(WARNING) << "Cannot deserialize message of type: '"
                 << message.message_type()
                 << "' with ID: " << message.message_id();
    return nullptr;
}

} // namespace events
} // namespace client
} // namespace one
