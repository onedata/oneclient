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

    auto fileChildrenAttrs = serverMessage->mutable_fuse_response()
                                 ->mutable_file_children_attrs()
                                 ->mutable_child_attrs();

    for (const auto &child : folly::range(fileChildrenAttrs->pointer_begin(),
             fileChildrenAttrs->pointer_end())) {
        m_childrenAttrs.emplace_back(*child);
    }
}

std::string FileChildrenAttrs::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileChildrenAttrs', uuids: [";

    for (const auto &attr : m_childrenAttrs)
        stream << attr.uuid() << " ";

    stream << "]";

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
