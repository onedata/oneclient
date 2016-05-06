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

GetFileLocation::GetFileLocation(
    std::string uuid, const one::helpers::FlagsSet flags)
    : m_uuid{std::move(uuid)}
    , m_flags{flags}
{
}

std::string GetFileLocation::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetFileLocation', uuid: " << m_uuid << ", flags: ";

    if (m_flags.find(one::helpers::Flag::RDWR) != m_flags.end())
        stream << "rdwr";
    else if (m_flags.find(one::helpers::Flag::RDONLY) != m_flags.end())
        stream << "read";
    else if (m_flags.find(one::helpers::Flag::WRONLY) != m_flags.end())
        stream << "write";

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileLocation::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto gfl = msg->mutable_fuse_request()->mutable_get_file_location();

    gfl->mutable_uuid()->swap(m_uuid);

    if (m_flags.find(one::helpers::Flag::RDWR) != m_flags.end())
        gfl->set_flags(clproto::FileLocationFlags::READ_WRITE);
    else if (m_flags.find(one::helpers::Flag::RDONLY) != m_flags.end())
        gfl->set_flags(clproto::FileLocationFlags::READ);
    else if (m_flags.find(one::helpers::Flag::WRONLY) != m_flags.end())
        gfl->set_flags(clproto::FileLocationFlags::WRITE);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
