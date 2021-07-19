/**
 * @file readSymLink.cc
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "readSymLink.h"

#include "messages.pb.h"
#include "spdlog/spdlog.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

ReadSymLink::ReadSymLink(folly::fbstring uuid)
    : FileRequest{uuid.toStdString()}
{
}

std::string ReadSymLink::toString() const
{
    return fmt::format("type: 'ReadSymLink', uuid: '{}'", m_contextGuid);
}

std::unique_ptr<ProtocolClientMessage> ReadSymLink::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    msg->mutable_fuse_request()->mutable_file_request()->mutable_read_symlink();

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
