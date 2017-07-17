/**
 * @file getFileLocation.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getFileLocation.h"

#include "messages.pb.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetFileLocation::GetFileLocation(std::string uuid)
    : FileRequest{std::move(uuid)}
{
}

std::string GetFileLocation::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetFileLocation', uuid: '" << m_contextGuid << "'";

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileLocation::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_get_file_location();

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
