/**
 * @file keyAggregator.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "keyAggregator.h"
#include "events/types/event.h"
#include "logging.h"

namespace one {
namespace client {
namespace events {

void KeyAggregator::process(ConstEventPtr event)
{
    auto it = m_events.find(event->aggregationKey());
    if (it != m_events.end()) {
        it->second->aggregate(std::move(event));
    }
    else {
        m_events.emplace(event->aggregationKey(), event->clone());
    }
}

std::vector<EventPtr> KeyAggregator::flush()
{
    std::vector<EventPtr> events;
    for (auto it = m_events.begin(); it != m_events.end(); ++it) {
        DLOG(INFO) << "Emitting event: " << it->second->toString();
        events.emplace_back(std::move(it->second));
    }
    m_events.clear();
    return events;
}

} // namespace events
} // namespace client
} // namespace one
