/**
 * @file release.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "release.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

Release::Release(std::string uuid, std::string handleId)
    : FileRequest{std::move(uuid)}
    , m_handleId{std::move(handleId)}
{
}

std::string Release::toString() const
{
    std::stringstream stream;
    stream << "type: 'Release', uuid: " << m_contextGuid
           << ", handleId: " << m_handleId;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Release::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto rm =
        msg->mutable_fuse_request()->mutable_file_request()->mutable_release();

    rm->mutable_handle_id()->swap(m_handleId);
    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
