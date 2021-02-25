/**
 * @file getChildAttr.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getChildAttr.h"

#include "messages.pb.h"

#include <cassert>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetChildAttr::GetChildAttr(folly::fbstring uuid, folly::fbstring name,
    folly::Optional<bool> includeReplicationStatus,
    folly::Optional<bool> includeLinkCount)
    : FileRequest{uuid.toStdString()}
    , m_name{std::move(name)}
    , m_includeReplicationStatus{std::move(includeReplicationStatus)}
    , m_includeLinkCount{std::move(includeLinkCount)}
{
}

std::string GetChildAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetChildAttr', uuid: " << m_contextGuid;
    stream << ", name: " << m_name;

    if (m_includeReplicationStatus)
        stream << ", includeReplicationStatus: " << *m_includeReplicationStatus;

    if (m_includeLinkCount)
        stream << ", includeLinkCount: " << *m_includeLinkCount;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetChildAttr::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_get_child_attr()
        ->set_name(m_name.toStdString());

    if (m_includeReplicationStatus)
        msg->mutable_fuse_request()
            ->mutable_file_request()
            ->mutable_get_child_attr()
            ->set_include_replication_status(*m_includeReplicationStatus);

    if (m_includeLinkCount)
        msg->mutable_fuse_request()
            ->mutable_file_request()
            ->mutable_get_child_attr()
            ->set_include_link_count(*m_includeLinkCount);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
