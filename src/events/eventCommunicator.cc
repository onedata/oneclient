/**
 * @file eventCommunicator.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/eventCommunicator.h"

#include "context.h"
#include "events/types/event.h"

namespace one {
namespace client {
namespace events {

EventCommunicator::EventCommunicator(std::shared_ptr<Context> ctx)
    : m_stmManager{new communication::StreamManager{ctx->communicator()}}
    , m_stm{m_stmManager->create()}
{
}

EventCommunicator::~EventCommunicator() { m_stm->close(); }

void EventCommunicator::send(const Event &evt) const { m_stm->send(evt); }

} // namespace events
} // namespace client
} // namespace one
