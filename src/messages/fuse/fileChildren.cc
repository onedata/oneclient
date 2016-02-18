/**
 * @file fileChildren.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileChildren.h"

#include "messages.pb.h"

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

    std::transform(fileChildren->pointer_begin(), fileChildren->pointer_end(),
        std::back_inserter(m_uuidsAndNames), [](auto child) {
            return std::make_tuple(std::move(*child->mutable_uuid()),
                std::move(*child->mutable_name()));
        });
}

const std::vector<std::tuple<std::string, std::string>> &
FileChildren::uuidsAndNames() const
{
    return m_uuidsAndNames;
}

std::string FileChildren::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileChildren', uuids: [";

    for (const auto &uuidAndName : m_uuidsAndNames)
        stream << "(" << std::get<0>(uuidAndName) << ", "
               << std::get<1>(uuidAndName) << ") ";

    stream << "]";

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
