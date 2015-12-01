/**
 * @file getNewFileLocation.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getNewFileLocation.h"

#include "messages.pb.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetNewFileLocation::GetNewFileLocation(
    std::string name, std::string parentUuid, mode_t mode)
    : m_name{std::move(name)}
    , m_parentUuid{std::move(parentUuid)}
    , m_mode{mode}
{
}

std::string GetNewFileLocation::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetNewFileLocation', name: " << m_name
           << ", parentUUID: " << m_parentUuid << ", mode: " << std::oct
           << m_mode;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetNewFileLocation::serialize() const
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto gnfl = msg->mutable_fuse_request()->mutable_get_new_file_location();

    gnfl->set_name(m_name);
    gnfl->set_parent_uuid(m_parentUuid);
    gnfl->set_mode(m_mode);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
