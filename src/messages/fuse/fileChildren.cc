/**
 * @file fileChildren.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileChildren.h"

#include "messages.pb.h"

#include <folly/Range.h>

#include <algorithm>
#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

FileChildren::FileChildren(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_file_children())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "file_children field missing"};

    auto fileChildren = serverMessage->mutable_fuse_response()
                            ->mutable_file_children()
                            ->mutable_child_links();

    for (const auto &child : folly::range(
             fileChildren->pointer_begin(), fileChildren->pointer_end())) {
        m_children.emplace_back(child->name());
    }
}

std::string FileChildren::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileChildren', uuids: [";

    for (const auto &name : m_children)
        stream << name << " ";

    stream << "]";

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
