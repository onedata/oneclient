/**
* @file eventEmissionRequest.cc
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
#include "events/messages/eventEmissionRequest.h"

namespace one {
namespace client {
namespace events {

EventEmissionRequest::EventEmissionRequest(unsigned long long id)
    : m_id{id}
{
}

void EventEmissionRequest::process(
    std::weak_ptr<EventBuffer> buffer,
    std::weak_ptr<EventCommunicator> communicator) const
{
    communicator.lock()->send(buffer.lock()->getSentMessage(m_id));
}

std::unique_ptr<EventEmissionRequest>
EventEmissionRequestSerializer::deserialize(const Message &message) const
{
    one::clproto::events::EventEmissionRequest eventEmissionRequest{};
    if (eventEmissionRequest.ParseFromString(message.worker_answer())) {
        return std::make_unique<one::client::events::EventEmissionRequest>(
            eventEmissionRequest.id());
    }
    LOG(WARNING) << "Cannot deserialize message of type: '"
                 << message.message_type()
                 << "' with ID: " << message.message_id();
    return nullptr;
}

} // namespace events
} // namespace client
} // namespace one
