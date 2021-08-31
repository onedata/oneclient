/**
 * @file getFileAttr.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getFileAttr.h"

#include "messages.pb.h"

#include <cassert>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetFileAttr::GetFileAttr(const folly::fbstring &uuid,
    bool includeReplicationStatus, bool includeLinkCount)
    : FileRequest{uuid.toStdString()}
    , m_includeReplicationStatus{includeReplicationStatus}
    , m_includeLinkCount{includeLinkCount}
{
}

std::string GetFileAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetFileAttr', uuid: " << m_contextGuid;

    if (m_includeReplicationStatus)
        stream << ", includeReplicationStatus: " << m_includeReplicationStatus;

    if (m_includeLinkCount)
        stream << ", includeLinkCount: " << m_includeLinkCount;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileAttr::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_get_file_attr();

    if (m_includeReplicationStatus)
        msg->mutable_fuse_request()
            ->mutable_file_request()
            ->mutable_get_file_attr()
            ->set_include_replication_status(m_includeReplicationStatus);

    if (m_includeLinkCount)
        msg->mutable_fuse_request()
            ->mutable_file_request()
            ->mutable_get_file_attr()
            ->set_include_link_count(m_includeLinkCount);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
