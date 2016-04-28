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
    std::string name, std::string parentUuid, mode_t mode, int flags)
    : m_name{std::move(name)}
    , m_parentUuid{std::move(parentUuid)}
    , m_mode{mode}
{
    if ((flags & O_ACCMODE) == O_RDONLY)
        m_flags = FileLocationFlags::read;
    else if ((flags & O_ACCMODE) == O_WRONLY)
        m_flags = FileLocationFlags::write;
    else if ((flags & O_ACCMODE) == O_RDWR)
        m_flags = FileLocationFlags::rdwr;
    else
        throw std::system_error{
            std::make_error_code(std::errc::protocol_error), "bad open flags"};
}

std::string GetNewFileLocation::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetNewFileLocation', name: " << m_name
           << ", parentUUID: " << m_parentUuid << ", mode: " << std::oct
           << m_mode << ", flags: ";
    switch (m_flags) {
        case FileLocationFlags::read:
            stream << "read";
            break;
        case FileLocationFlags::write:
            stream << "write";
            break;
        case FileLocationFlags::rdwr:
            stream << "rdwr";
            break;
    };

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetNewFileLocation::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto gnfl = msg->mutable_fuse_request()->mutable_get_new_file_location();

    gnfl->mutable_name()->swap(m_name);
    gnfl->mutable_parent_uuid()->swap(m_parentUuid);
    gnfl->set_mode(m_mode);
    switch (m_flags) {
        case FileLocationFlags::read:
            gnfl->set_flags(clproto::FileLocationFlags::READ);
            break;
        case FileLocationFlags::write:
            gnfl->set_flags(clproto::FileLocationFlags::WRITE);
            break;
        case FileLocationFlags::rdwr:
            gnfl->set_flags(clproto::FileLocationFlags::READ_WRITE);
            break;
    }

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
