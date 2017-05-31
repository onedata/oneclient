/**
 * @file fsync.cc
 * @author Tomasz Lichon
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsync.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

FSync::FSync(std::string uuid, bool dataOnly, std::string handleId)
    : FileRequest{std::move(uuid)}
    , m_dataOnly{std::move(dataOnly)}
    , m_handleId{std::move(handleId)}
{
}

std::string FSync::toString() const
{
    std::stringstream stream;
    stream << "type: 'FSync', uuid: " << m_contextGuid
           << ", dataOnly: " << m_dataOnly << ", handleId: " << m_handleId;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> FSync::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto fsync =
        msg->mutable_fuse_request()->mutable_file_request()->mutable_fsync();

    fsync->set_data_only(m_dataOnly);
    fsync->mutable_handle_id()->swap(m_handleId);
    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
