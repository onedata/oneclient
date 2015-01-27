/**
* @file eventCommunicator.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"
#include "logging.h"

#include "events/eventCommunicator.h"
#include "communication/exception.h"
#include "communication/communicator.h"

namespace one {
namespace client {
namespace events {

EventCommunicator::EventCommunicator(std::shared_ptr<Context> context)
    : m_context{std::move(context)}
{
}

void EventCommunicator::send(const google::protobuf::Message &message) const
{
    try {
        m_context->getCommunicator()->send(
            communication::ServerModule::EVENT_MANAGER, message);
    }
    catch (const communication::Exception &e) {
        LOG(WARNING) << "Cannot send event message due to: " << e.what();
    }
}

} // namespace events
} // namespace client
} // namespace one
