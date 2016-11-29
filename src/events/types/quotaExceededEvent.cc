/**
 * @file quotaExceededEvent.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "quotaExceededEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

QuotaExceededEvent::QuotaExceededEvent(const ProtocolMessage &msg)
{
    for (const auto &spaceId : msg.spaces()) {
        m_spaces.emplace_back(spaceId);
    }
}

const std::string &QuotaExceededEvent::routingKey() const
{
    return m_routingKey;
}

const std::string &QuotaExceededEvent::aggregationKey() const
{
    return m_routingKey;
}

const std::vector<std::string> &QuotaExceededEvent::spaces() const
{
    return m_spaces;
}

std::string QuotaExceededEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'QuotaExceededEvent', spaces: [";
    for (const auto &spaceId : m_spaces) {
        stream << "'" << spaceId << "', ";
    }
    stream << "]";
    return stream.str();
}

void QuotaExceededEvent::aggregate(ConstEventPtr event) {}

EventPtr QuotaExceededEvent::clone() const
{
    return std::make_shared<QuotaExceededEvent>(*this);
}

} // namespace events
} // namespace client
} // namespace one
