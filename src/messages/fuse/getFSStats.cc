/**
 * @file getFSStats.cc
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getFSStats.h"

#include "messages.pb.h"

namespace one {
namespace messages {
namespace fuse {

GetFSStats::GetFSStats(std::string fileId)
    : m_fileId{std::move(fileId)}
{
}

std::unique_ptr<ProtocolClientMessage> GetFSStats::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();

    auto cg =
        msg->mutable_fuse_request()->mutable_get_fs_stats()->mutable_file_id();
    cg->swap(m_fileId);

    return msg;
}

std::string GetFSStats::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetFSStats', file_id: '" << m_fileId << "'";

    return stream.str();
}
} // namespace fuse
} // namespace messages
} // namespace one
