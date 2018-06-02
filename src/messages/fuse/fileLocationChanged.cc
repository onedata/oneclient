/**
 * @file fileLocationChanged.cc
 * @author Bartek Kryza
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileLocationChanged.h"

#include "messages.pb.h"

#include <sstream>
#include <system_error>

using namespace std::literals;

namespace one {
namespace messages {
namespace fuse {

FileLocationChanged::FileLocationChanged(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_file_location_changed())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_location_changed field missing"};

    deserialize(serverMessage->fuse_response().file_location_changed());
}

FileLocationChanged::FileLocationChanged(const ProtocolMessage &message)
{
    deserialize(message);
}

const FileLocation &FileLocationChanged::fileLocation() const
{
    return *m_fileLocation;
}

boost::optional<off_t> FileLocationChanged::changeStartOffset() const
{
    return m_changeBegOffset;
}

boost::optional<off_t> FileLocationChanged::changeEndOffset() const
{
    return m_changeEndOffset;
}

std::string FileLocationChanged::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileLocationChanged', file_location: '"
           << m_fileLocation->toString();

    if (m_changeBegOffset && m_changeEndOffset) {
        stream << ", change_beg_offset: " << *m_changeBegOffset;
        stream << ", change_end_offset: " << *m_changeEndOffset;
    }

    return stream.str();
}

void FileLocationChanged::deserialize(const ProtocolMessage &message)
{
    m_fileLocation = std::make_unique<FileLocation>(message.file_location());

    if (message.has_change_beg_offset())
        m_changeBegOffset = message.change_beg_offset();

    if (message.has_change_end_offset())
        m_changeEndOffset = message.change_end_offset();
}

} // namespace fuse
} // namespace messages
} // namespace one
