/**
 * @file permissionChangedEvent.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "permissionChangedEvent.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

PermissionChangedEvent::PermissionChangedEvent(const ProtocolMessage &message)
{
    m_fileUuid = message.file_uuid();
}

const PermissionChangedEvent::Key &PermissionChangedEvent::key() const { return m_fileUuid; }

const std::string &PermissionChangedEvent::fileUuid() const { return m_fileUuid; }

void PermissionChangedEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
}

std::string PermissionChangedEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'PermissionChangedEvent', counter: " << m_counter << ", file UUID: '"
           << m_fileUuid;
    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> PermissionChangedEvent::serializeAndDestroy()
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    auto permissionChangedEventMsg = eventMsg->mutable_permission_changed_event();
    eventMsg->set_counter(m_counter);
    permissionChangedEventMsg->mutable_file_uuid()->swap(m_fileUuid);

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
