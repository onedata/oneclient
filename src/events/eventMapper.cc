/**
* @file eventMapper.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"

#include "events/eventMapper.h"
#include "events/eventBuffer.h"
#include "events/types/event.h"

namespace one {
namespace client {
namespace events {

EventMapper::EventMapper(std::shared_ptr<Context> context)
    : m_context{std::move(context)}
{
}

void EventMapper::map(const Event &event) {}

const std::string &
EventMapper::addOrUpdateEventStream(const EventStream &stream)
{
    return stream.id();
}

bool EventMapper::removeEventStream(const std::string &id) { return false; }

} // namespace events
} // namespace client
} // namespace one
