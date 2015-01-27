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

#include "events/eventManager.h"
#include "events/messages/eventEmissionRequest.h"

namespace one {
namespace client {
namespace events {

EventEmissionRequest::EventEmissionRequest(unsigned long long id)
    : m_id{id}
{
}

bool EventEmissionRequest::process(EventManager &manager) const
{
    manager.emit(m_id);
    return true;
}

std::unique_ptr<EventMessage>
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
