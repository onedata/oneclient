/**
 * @file fileRenamedEvent.cc
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRenamedEvent.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRenamedEvent::FileRenamedEvent(const ProtocolMessage &message)
    : m_oldUuid{message.old_uuid()}
    , m_newUuid{message.new_uuid()}
{
}

const FileRenamedEvent::Key &FileRenamedEvent::key() const
{
    return m_oldUuid;
}

const std::string &FileRenamedEvent::oldUuid() const { return m_oldUuid; }

const std::string &FileRenamedEvent::newUuid() const { return m_newUuid; }

void FileRenamedEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
    m_newUuid = event->newUuid();
}

std::string FileRenamedEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRenamedEvent', counter: " << m_counter
           << ", old file UUID: '" << m_oldUuid
           << ", new file UUID: '" << m_newUuid;
    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> FileRenamedEvent::serializeAndDestroy()
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    auto fileRenamedEventMsg = eventMsg->mutable_file_renamed_event();
    eventMsg->set_counter(m_counter);
    fileRenamedEventMsg->mutable_old_uuid()->swap(m_oldUuid);
    fileRenamedEventMsg->mutable_new_uuid()->swap(m_newUuid);

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
