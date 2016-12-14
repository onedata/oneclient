/**
 * @file quotaExceeded.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "quotaExceeded.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

QuotaExceeded::QuotaExceeded(const ProtocolMessage &msg)
{
    for (const auto &spaceId : msg.spaces()) {
        m_spaces.emplace_back(spaceId);
    }
}

StreamKey QuotaExceeded::streamKey() const
{
    return StreamKey::QUOTA_EXCEEDED;
}

const std::vector<std::string> &QuotaExceeded::spaces() const
{
    return m_spaces;
}

std::string QuotaExceeded::toString() const
{
    std::stringstream stream;
    stream << "type: 'QuotaExceeded', spaces: [";
    for (const auto &spaceId : m_spaces) {
        stream << "'" << spaceId << "', ";
    }
    stream << "]";
    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
