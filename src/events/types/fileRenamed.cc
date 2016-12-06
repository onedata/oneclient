/**
 * @file fileRenamed.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRenamed.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace client {
namespace events {

FileRenamed::FileRenamed(const ProtocolMessage &msg)
    : m_topEntry{msg.top_entry()}
{
    for (auto &childEntry : msg.child_entries()) {
        m_childEntries.emplace_back(childEntry);
    }
}

StreamKey FileRenamed::streamKey() const { return StreamKey::FILE_RENAMED; }

const FileRenamed::FileRenamedEntry &FileRenamed::topEntry() const
{
    return m_topEntry;
}

const std::vector<FileRenamed::FileRenamedEntry> &
FileRenamed::childEntries() const
{
    return m_childEntries;
}

std::string FileRenamed::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRenamed', top entry: " << m_topEntry.toString()
           << ", child entries: [";
    for (const auto &childEntry : m_childEntries)
        stream << childEntry.toString() << ", ";
    stream << "]";

    return stream.str();
}

} // namespace events
} // namespace client
} // namespace one
