/**
 * @file remoteHandler.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/events.h"

#include "messages.pb.h"

#include <cassert>

namespace one {
namespace client {
namespace events {

RemoteHandler::RemoteHandler(SequencerStreamPtr sequencerStream)
    : m_sequencerStream{std::move(sequencerStream)}
{
}

void RemoteHandler::process(std::vector<EventPtr> events)
{
    auto clientMsg = std::make_unique<one::clproto::ClientMessage>();
    auto eventsMsg = clientMsg->mutable_events();

    for (auto event : events) {
        auto remoteEvent = events::get<RemoteEvent>(event);
        auto eventMsg = eventsMsg->add_events();
        eventMsg->Swap(remoteEvent->serializeAndDestroy().release());
    }

    m_sequencerStream->send(std::move(clientMsg));
}

} // namespace events
} // namespace client
} // namespace one
