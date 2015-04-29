/**
* @file eventCommunicator.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"
#include "events/types/event.h"
#include "events/eventCommunicator.h"

#include <glog/logging.h>

namespace one {
namespace client {
namespace events {

EventCommunicator::EventCommunicator(std::weak_ptr<Context> context)
    : m_context{std::move(context)}
{
    m_streamManager = std::make_unique<communication::StreamManager>(
        m_context.lock()->communicator());
    m_stream = m_streamManager->create();
}

EventCommunicator::~EventCommunicator() { m_stream->close(); }

void EventCommunicator::send(const Event &event) const
{
    DLOG(INFO) << "Sending event: " << event.toString();
    m_stream->send(event.serialize(), 0);
}

} // namespace events
} // namespace client
} // namespace one
