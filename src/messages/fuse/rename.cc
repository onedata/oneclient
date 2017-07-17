/**
 * @file rename.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "rename.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

Rename::Rename(
    std::string uuid, std::string targetParentUuid, std::string targetName)
    : FileRequest{std::move(uuid)}
    , m_targetParentUuid{std::move(targetParentUuid)}
    , m_targetName{std::move(targetName)}
{
}

std::string Rename::toString() const
{
    std::stringstream stream;
    stream << "type: 'Rename', uuid: " << m_contextGuid;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Rename::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto rnm =
        msg->mutable_fuse_request()->mutable_file_request()->mutable_rename();

    rnm->mutable_target_parent_uuid()->swap(m_targetParentUuid);
    rnm->mutable_target_name()->swap(m_targetName);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
