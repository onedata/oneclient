/**
 * @file eventSubscriptionCancellation.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/subscriptions/eventSubscriptionCancellation.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

EventSubscriptionCancellation::EventSubscriptionCancellation(
    const one::clproto::EventSubscriptionCancellation &subCan)
{
    m_id = subCan.id();
}

EventSubscriptionCancellation::EventSubscriptionCancellation(uint64_t id_)
    : m_id{id_}
{
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
