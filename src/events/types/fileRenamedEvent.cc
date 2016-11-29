/**
 * @file fileRenamedEvent.cc
 * @author Krzysztof Trzepla
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

FileRenamedEvent::FileRenamedEvent(const ProtocolMessage &msg)
    : m_topEntry{msg.top_entry()}
    , m_routingKey{"FileRenamedEventStream." + m_topEntry.oldUuid()}
{
    for (auto &childEntry : msg.child_entries()) {
        m_childEntries.emplace_back(childEntry);
    }
}

const std::string &FileRenamedEvent::routingKey() const { return m_routingKey; }

const std::string &FileRenamedEvent::aggregationKey() const
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

std::string FileRenamedEvent::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRenamed', top entry: " << m_topEntry.toString()
           << ", child entries: [";
    for (const auto &childEntry : m_childEntries)
        stream << childEntry.toString() << ", ";
    stream << "]";

    return stream.str();
}

void FileRenamedEvent::aggregate(ConstEventPtr event) {}

EventPtr FileRenamedEvent::clone() const
{
    return std::make_shared<FileRenamedEvent>(*this);
}

} // namespace events
} // namespace client
} // namespace one
