/**
 * @file fileRemovedEvent.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRemovedEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRemovedEvent::FileRemovedEvent(const ProtocolMessage &message)
    : FileRemovedEvent::FileRemovedEvent(message.file_uuid())
{
}

FileRemovedEvent::FileRemovedEvent(std::string fileUuid)
    : m_fileUuid(fileUuid)
{
}

const FileRemovedEvent::Key &FileRemovedEvent::key() const
{
    return m_fileUuid;
}

const std::string &FileRemovedEvent::fileUuid() const { return m_fileUuid; }

void FileRemovedEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
}

std::string FileRemovedEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRemovedEvent', counter: " << m_counter
           << ", file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> FileRemovedEvent::serializeAndDestroy()
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    auto fileRemovedEventMsg = eventMsg->mutable_file_removed_event();
    eventMsg->set_counter(m_counter);
    fileRemovedEventMsg->mutable_file_uuid()->swap(m_fileUuid);

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
