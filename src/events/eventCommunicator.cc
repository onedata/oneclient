/**
* @file eventCommunicator.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"
#include "events/eventCommunicator.h"

namespace one {
namespace client {
namespace events {

EventCommunicator::EventCommunicator(std::weak_ptr<Context> context)
    : m_context{std::move(context)}
{
}

void EventCommunicator::send(const Event &event) const {}

} // namespace events
} // namespace client
} // namespace one
