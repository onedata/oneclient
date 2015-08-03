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

Rename::Rename(std::string uuid, boost::filesystem::path targetPath)
    : m_uuid{std::move(uuid)}
    , m_targetPath{std::move(targetPath)}
{
}

std::string Rename::toString() const
{
    std::stringstream stream;
    stream << "type: 'Rename', uuid: " << m_uuid
           << ", targetPath: " << m_targetPath;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Rename::serialize() const
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto r = msg->mutable_fuse_request()->mutable_rename();

    r->set_uuid(m_uuid);
    r->set_target_path(m_targetPath.string());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
