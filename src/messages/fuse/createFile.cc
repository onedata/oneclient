/**
 * @file createFile.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "createFile.h"

#include "messages.pb.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

CreateFile::CreateFile(folly::fbstring parentUuid, folly::fbstring name,
    const mode_t mode, const one::helpers::Flag flag)
    : FileRequest{parentUuid.toStdString()}
    , m_name{std::move(name)}
    , m_mode{mode}
    , m_flag{flag}
{
}

std::string CreateFile::toString() const
{
    std::stringstream stream;

    stream << "type: 'CreateFile', name: '" << m_name << "', parentUUID: '"
           << m_contextGuid << "', mode: " << std::oct << m_mode << ", flag: ";

    if (m_flag == one::helpers::Flag::RDONLY)
        stream << "read";
    else if (m_flag == one::helpers::Flag::WRONLY)
        stream << "write";
    else
        stream << "rdwr";

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> CreateFile::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto cf = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_create_file();

    cf->set_name(m_name.toStdString());
    cf->set_mode(m_mode);

    if (m_flag == one::helpers::Flag::RDONLY)
        cf->set_flag(clproto::OpenFlag::READ);
    else if (m_flag == one::helpers::Flag::WRONLY)
        cf->set_flag(clproto::OpenFlag::WRITE);
    else
        cf->set_flag(clproto::OpenFlag::READ_WRITE);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
