/**
 * @file fileRenamed.cc
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileRenamed.h"

#include "messages.pb.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

FileRenamed::FileRenamed(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_file_renamed())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_renamed field missing"};

    auto fileRenamed =
        serverMessage->mutable_fuse_response()->mutable_file_renamed();

    m_newUuid.swap(*fileRenamed->mutable_new_uuid());

    for (auto &childEntry : fileRenamed->child_entries()) {
        m_childEntries.push_back(FileRenamedEntry{childEntry});
    }
}

const std::string &FileRenamed::newUuid() const { return m_newUuid; }

const std::vector<FileRenamedEntry> &FileRenamed::childEntries() const
{
    return m_childEntries;
}

std::string FileRenamed::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRenamedEvent', new uuid: " << m_newUuid
           << ", child entries: [";

    for (const auto &childEntry : m_childEntries)
        stream << childEntry.toString() << ", ";

    stream << "]";

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
