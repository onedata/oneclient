/**
 * @file createPath.cc
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "createPath.h"

#include "messages.pb.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

CreatePath::CreatePath(const folly::fbstring &parentUuid, folly::fbstring path)
    : FileRequest{parentUuid.toStdString()}
    , m_path{std::move(path)}
{
}

std::string CreatePath::toString() const
{
    std::stringstream stream;

    stream << "type: 'CreatePath', path: '" << m_path << "', parentUUID: '"
           << m_contextGuid;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> CreatePath::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto *cf = msg->mutable_fuse_request()
                   ->mutable_file_request()
                   ->mutable_create_path();

    cf->set_path(m_path.toStdString());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
