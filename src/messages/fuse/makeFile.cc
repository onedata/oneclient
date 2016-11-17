/**
 * @file makeFile.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "makeFile.h"

#include "messages.pb.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

MakeFile::MakeFile(
    folly::fbstring parentUuid, folly::fbstring name, const mode_t mode)
    : FileRequest{parentUuid.toStdString()}
    , m_name{std::move(name)}
    , m_mode{mode}
{
}

std::string MakeFile::toString() const
{
    std::stringstream stream;

    stream << "type: 'MakeFile', name: '" << m_name << "', parentUUID: '"
           << m_contextGuid << "', mode: " << std::oct << m_mode;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> MakeFile::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto mf = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_make_file();

    mf->set_name(m_name.toStdString());
    mf->set_mode(m_mode);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
