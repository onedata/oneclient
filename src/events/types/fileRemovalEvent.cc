/**
 * @file fileRemovalEvent.cc
 * @author Michal Wrona
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRemovalEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRemovalEvent::FileRemovalEvent(const ProtocolMessage &message)
    : FileRemovalEvent::FileRemovalEvent(message.file_uuid())
{
}

FileRemovalEvent::FileRemovalEvent(std::string fileUuid)
    : m_fileUuid(fileUuid)
{
}

const FileRemovalEvent::Key &FileRemovalEvent::key() const
{
    return m_fileUuid;
}

const std::string &FileRemovalEvent::fileUuid() const { return m_fileUuid; }

void FileRemovalEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
}

std::string FileRemovalEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRemovalEvent', counter: " << m_counter
           << ", file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> FileRemovalEvent::serializeAndDestroy()
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    auto fileRemovalEventMsg = eventMsg->mutable_file_removal_event();
    eventMsg->set_counter(m_counter);
    fileRemovalEventMsg->mutable_file_uuid()->swap(m_fileUuid);

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
