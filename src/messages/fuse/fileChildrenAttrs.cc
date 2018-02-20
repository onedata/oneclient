/**
 * @file fileChildrenAttrs.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileChildrenAttrs.h"

#include "messages.pb.h"

#include <folly/Range.h>

#include <algorithm>
#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

FileChildrenAttrs::FileChildrenAttrs(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_file_children_attrs())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_children_attrs field missing"};

    auto fileChildrenAttrs =
        serverMessage->mutable_fuse_response()->mutable_file_children_attrs();

    auto childAttrs = fileChildrenAttrs->mutable_child_attrs();

    for (const auto &child :
        folly::range(childAttrs->pointer_begin(), childAttrs->pointer_end())) {
        m_childrenAttrs.emplace_back(*child);
    }

    if (fileChildrenAttrs->has_index_token()) {
        m_indexToken = fileChildrenAttrs->index_token();
    }

    if (fileChildrenAttrs->has_is_last()) {
        m_isLast = fileChildrenAttrs->is_last();
    }
}

std::string FileChildrenAttrs::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileChildrenAttrs', uuids: [";

    for (const auto &attr : m_childrenAttrs)
        stream << attr.uuid() << " ";

    stream << "]";

    if (m_indexToken)
        stream << ", index_token: " << *m_indexToken;

    if (m_isLast)
        stream << ", is_last: " << *m_isLast;

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
