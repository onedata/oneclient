/**
 * @file quotaExeededEvent.cc
 * @author Rafal Slota
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "quotaExeededEvent.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

QuotaExeededEvent::QuotaExeededEvent(const ProtocolMessage &message)
{
    for(const auto& spaceId: message.spaces()) {
        m_spaces.push_back(spaceId);
    }
}

const QuotaExeededEvent::Key &QuotaExeededEvent::key() const { return m_key; }

const std::vector<std::string> &QuotaExeededEvent::spaces() const { return m_spaces; }

void QuotaExeededEvent::aggregate(EventPtr event)
{
    m_spaces = event->spaces();
}

std::string QuotaExeededEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'QuotaExeededEvent'";
    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> QuotaExeededEvent::serializeAndDestroy()
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    eventMsg->mutable_quota_exeeded_event();
    eventMsg->set_counter(m_counter);

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
