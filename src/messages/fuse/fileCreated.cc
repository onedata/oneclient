/**
 * @file fileCreated.cc
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileCreated.h"
#include "fileAttr.h"
#include "fileLocation.h"

#include "messages.pb.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

FileCreated::FileCreated(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_file_created())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_created field missing"};

    auto fileCreated =
        serverMessage->mutable_fuse_response()->mutable_file_created();

    fileCreated->mutable_handle_id()->swap(m_handleId);
    m_attr = std::make_unique<FileAttr>(fileCreated->file_attr());
    m_location = std::make_unique<FileLocation>(fileCreated->file_location());
}

const std::string &FileCreated::handleId() const { return m_handleId; }

const FileAttr &FileCreated::attr() const { return *m_attr; }

const FileLocation &FileCreated::location() const { return *m_location; }

std::string FileCreated::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileCreated', handleId: '" << m_handleId
           << "', attr: " << m_attr->toString()
           << ", location: " << m_location->toString();

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
