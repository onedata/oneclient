/**
 * @file removeFileEvent.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "removeFileEvent.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

RemoveFileEvent::RemoveFileEvent(const ProtocolMessage &message)
{
    m_fileUuid = message.file_uuid();
}

RemoveFileEvent::RemoveFileEvent(std::string fileUuid)
    : m_fileUuid(fileUuid)
{
}

const RemoveFileEvent::Key &RemoveFileEvent::key() const { return m_fileUuid; }

const std::string &RemoveFileEvent::fileUuid() const { return m_fileUuid; }

void RemoveFileEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
}

std::string RemoveFileEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'RemoveFileEvent', counter: " << m_counter
           << ", file UUID: '" << m_fileUuid;
    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> RemoveFileEvent::serializeAndDestroy()
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    auto removeFileEventMsg = eventMsg->mutable_remove_file_event();
    eventMsg->set_counter(m_counter);
    removeFileEventMsg->mutable_file_uuid()->swap(m_fileUuid);

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
