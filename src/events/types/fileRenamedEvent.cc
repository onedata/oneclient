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
{
    m_topEntry = FileRenamedEntry{message.top_entry()};

    for (auto &childEntry : message.child_entries()) {
        m_childEntries.push_back(FileRenamedEntry{childEntry});
    }
}

const FileRenamedEvent::Key &FileRenamedEvent::key() const
{
    return m_topEntry.oldUuid();
}

const FileRenamedEvent::FileRenamedEntry &FileRenamedEvent::topEntry() const
{
    return m_topEntry;
}

const std::vector<FileRenamedEvent::FileRenamedEntry> &
FileRenamedEvent::childEntries() const
{
    return m_childEntries;
}

void FileRenamedEvent::aggregate(EventPtr event)
{
    m_counter += event->m_counter;
}

std::string FileRenamedEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRenamedEvent', counter: " << m_counter
           << ", top entry: " << m_topEntry.toString() << ", child entries: [";

    for (const auto &childEntry : m_childEntries)
        stream << childEntry.toString() << ", ";

    stream << "]";

    return stream.str();
}

std::unique_ptr<ProtocolEventMessage> FileRenamedEvent::serializeAndDestroy()
{
    auto eventMsg = std::make_unique<ProtocolEventMessage>();
    auto fileRenamedEventMsg = eventMsg->mutable_file_renamed_event();
    eventMsg->set_counter(m_counter);

    m_topEntry.fillProtocolMessage(*fileRenamedEventMsg->mutable_top_entry());

    for (auto &childEntry : m_childEntries)
        childEntry.fillProtocolMessage(
            *fileRenamedEventMsg->add_child_entries());

    return eventMsg;
}

} // namespace events
} // namespace client
} // namespace one
