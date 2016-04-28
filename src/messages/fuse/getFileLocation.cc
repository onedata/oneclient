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

GetFileLocation::GetFileLocation(std::string uuid, int flags)
    : m_uuid{std::move(uuid)}
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

std::string GetFileLocation::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetFileLocation', uuid: " << m_uuid << ", flags: ";
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
    }
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileLocation::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto gfl = msg->mutable_fuse_request()->mutable_get_file_location();

    gfl->mutable_uuid()->swap(m_uuid);

    switch (m_flags) {
        case FileLocationFlags::read:
            gfl->set_flags(clproto::FileLocationFlags::READ);
            break;
        case FileLocationFlags::write:
            gfl->set_flags(clproto::FileLocationFlags::WRITE);
            break;
        case FileLocationFlags::rdwr:
            gfl->set_flags(clproto::FileLocationFlags::READ_WRITE);
            break;
    }

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
