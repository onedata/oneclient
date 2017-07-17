/**
 * @file fileOpened.cc
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileOpened.h"

#include "messages.pb.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

FileOpened::FileOpened(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_file_opened())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_opened field missing"};

    auto fileOpened =
        serverMessage->mutable_fuse_response()->mutable_file_opened();

    fileOpened->mutable_handle_id()->swap(m_handleId);
}

const std::string &FileOpened::handleId() const { return m_handleId; }

std::string FileOpened::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileOpened', handleId: '" << m_handleId << "'";

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
