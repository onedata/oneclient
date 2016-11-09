/**
 * @file syncResponse.cc
 * @author Tomasz Lichon
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "syncResponse.h"

#include "messages.pb.h"
#include "messages/status.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

SyncResponse::SyncResponse(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_sync_response())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "sync_response field missing"};

    auto &message =
        *serverMessage->mutable_fuse_response()->mutable_sync_response();

    message.mutable_checksum()->swap(m_checksum);
    m_fileLocation = FileLocation(*message.mutable_file_location());
}

std::string SyncResponse::toString() const
{
    std::stringstream ss;
    ss << "type: 'SyncResponse', checksum: '" << m_checksum
       << "', file_location: " + m_fileLocation.toString();
    return ss.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
