/**
 * @file openFile.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "openFile.h"

#include "messages.pb.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

OpenFile::OpenFile(std::string uuid, const one::helpers::Flag flag)
    : FileRequest{std::move(uuid)}
    , m_flag{flag}
{
}

std::string OpenFile::toString() const
{
    std::stringstream stream;

    stream << "type: 'OpenFile', uuid: '" << m_contextGuid << "', flag: ";

    if (m_flag == one::helpers::Flag::RDONLY)
        stream << "read";
    else if (m_flag == one::helpers::Flag::WRONLY)
        stream << "write";
    else
        stream << "rdwr";

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> OpenFile::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto of = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_open_file();

    if (m_flag == one::helpers::Flag::RDONLY)
        of->set_flag(clproto::OpenFlag::READ);
    else if (m_flag == one::helpers::Flag::WRONLY)
        of->set_flag(clproto::OpenFlag::WRITE);
    else
        of->set_flag(clproto::OpenFlag::READ_WRITE);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
