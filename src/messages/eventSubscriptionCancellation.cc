/**
 * @file eventSubscriptionCancellation.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/eventSubscriptionCancellation.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

EventSubscriptionCancellation::EventSubscriptionCancellation(
    const messages::ProtocolServerMessage &serverMessage)
{
    auto &eventSubscriptionMsg = serverMessage.event_subscription();
    auto &eventSubscriptionCancellationMsg =
        eventSubscriptionMsg.event_subscription_cancellation();
    m_id = eventSubscriptionCancellationMsg.id();
}

uint64_t EventSubscriptionCancellation::id() const { return m_id; }

std::string EventSubscriptionCancellation::toString() const
{
    std::stringstream stream;
    stream << "type: 'EventSubscriptionCancellation', id: " << m_id;
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
