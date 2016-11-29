/**
 * @file localHandler.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "localHandler.h"

namespace one {
namespace client {
namespace events {

LocalHandler::LocalHandler(EventHandler handler)
    : m_handler{std::move(handler)}
{
}

void LocalHandler::process(std::vector<EventPtr> events)
{
    if (!events.empty()) {
        m_handler(std::move(events));
    }
}

} // namespace events
} // namespace client
} // namespace one
