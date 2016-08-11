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
    : FileRequest{std::move(parentUuid)}
    , m_name{std::move(name)}
    , m_mode{mode}
    , m_flags{flags}
{
}

std::string GetNewFileLocation::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetNewFileLocation', name: " << m_name
           << ", parentUUID: " << m_contextGuid << ", mode: " << std::oct
           << m_mode << ", flags: ";

    if (m_flags.count(one::helpers::Flag::RDWR))
        stream << "rdwr";
    else if (m_flags.count(one::helpers::Flag::RDONLY))
        stream << "read";
    else if (m_flags.count(one::helpers::Flag::WRONLY))
        stream << "write";

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetNewFileLocation::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto gnfl = msg->mutable_fuse_request()
                    ->mutable_file_request()
                    ->mutable_get_new_file_location();

    gnfl->mutable_name()->swap(m_name);
    gnfl->set_mode(m_mode);

    if (m_flags.count(one::helpers::Flag::RDWR))
        gnfl->set_flags(clproto::FileLocationFlags::READ_WRITE);
    else if (m_flags.count(one::helpers::Flag::RDONLY))
        gnfl->set_flags(clproto::FileLocationFlags::READ);
    else if (m_flags.count(one::helpers::Flag::WRONLY))
        gnfl->set_flags(clproto::FileLocationFlags::WRITE);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
