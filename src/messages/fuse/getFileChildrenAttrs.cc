/**
 * @file getFileChildrenAttrs.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getFileChildrenAttrs.h"

#include "messages.pb.h"

#include <fmt/format.h>

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetFileChildrenAttrs::GetFileChildrenAttrs(
    const folly::fbstring &uuid, const off_t offset, const std::size_t size)
    : FileRequest{uuid.toStdString()}
    , m_offset{offset}
    , m_size{size}
{
}

GetFileChildrenAttrs::GetFileChildrenAttrs(const folly::fbstring &uuid,
    const off_t offset, const std::size_t size,
    folly::Optional<folly::fbstring> indexToken, bool includeReplicationStatus,
    bool includeHardLinkCount, std::vector<std::string> xattrs)
    : GetFileChildrenAttrs{uuid, offset, size}
{
    m_includeReplicationStatus = includeReplicationStatus;
    m_includeHardLinkCount = includeHardLinkCount;
    m_indexToken.assign(std::move(indexToken));
    m_xattrs = std::move(xattrs);
}

std::string GetFileChildrenAttrs::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetFileChildrenAttrs', uuid: " << m_contextGuid
           << ", offset: " << m_offset << ", size: " << m_size;

    if (m_indexToken)
        stream << ", index_token: " << *m_indexToken;

    if (m_includeReplicationStatus)
        stream << ", include_replication_status: "
               << m_includeReplicationStatus;

    if (m_includeHardLinkCount)
        stream << ", include_hard_link_count: " << m_includeHardLinkCount;

    if (!m_xattrs.empty())
        stream << ", xattrs: " << fmt::format("({})", fmt::join(m_xattrs, ","));

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage>
GetFileChildrenAttrs::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto *gfc = msg->mutable_fuse_request()
                    ->mutable_file_request()
                    ->mutable_get_file_children_attrs();

    gfc->set_offset(m_offset);
    gfc->set_size(m_size);

    if (m_indexToken)
        gfc->set_index_token(m_indexToken->toStdString());

    if (m_includeReplicationStatus)
        gfc->set_include_replication_status(m_includeReplicationStatus);

    if (m_includeHardLinkCount)
        gfc->set_include_link_count(m_includeReplicationStatus);

    if (!m_xattrs.empty()) {
        for (auto &xattr : m_xattrs)
            gfc->add_xattrs(std::move(xattr));
    }

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
