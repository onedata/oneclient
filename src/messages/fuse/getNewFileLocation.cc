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

GetNewFileLocation::GetNewFileLocation(std::string name, std::string parentUuid,
    mode_t mode, const one::helpers::FlagsSet flags)
    : m_name{std::move(name)}
    , m_parentUuid{std::move(parentUuid)}
    , m_mode{mode}
    , m_flags{flags}
{
}

std::string GetNewFileLocation::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetNewFileLocation', name: " << m_name
           << ", parentUUID: " << m_parentUuid << ", mode: " << std::oct
           << m_mode << ", flags: ";

    if (m_flags.find(one::helpers::Flag::RDWR) != m_flags.end())
        stream << "rdwr";
    else if (m_flags.find(one::helpers::Flag::RDONLY) != m_flags.end())
        stream << "read";
    else if (m_flags.find(one::helpers::Flag::WRONLY) != m_flags.end())
        stream << "write";

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetNewFileLocation::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto gnfl = msg->mutable_fuse_request()->mutable_get_new_file_location();

    gnfl->mutable_name()->swap(m_name);
    gnfl->mutable_parent_uuid()->swap(m_parentUuid);
    gnfl->set_mode(m_mode);

    if (m_flags.find(one::helpers::Flag::RDWR) != m_flags.end())
        gnfl->set_flags(clproto::FileLocationFlags::READ_WRITE);
    else if (m_flags.find(one::helpers::Flag::RDONLY) != m_flags.end())
        gnfl->set_flags(clproto::FileLocationFlags::READ);
    else if (m_flags.find(one::helpers::Flag::WRONLY) != m_flags.end())
        gnfl->set_flags(clproto::FileLocationFlags::WRITE);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
