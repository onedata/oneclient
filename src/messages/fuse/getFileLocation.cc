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
    : m_uuid{std::move(uuid)}
{
}

std::string GetFileLocation::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetFileLocation', uuid: " << m_uuid;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileLocation::serialize() const
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto gfl = msg->mutable_fuse_request()->mutable_get_file_location();

    gfl->set_uuid(m_uuid);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
