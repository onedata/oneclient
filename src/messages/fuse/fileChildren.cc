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

namespace one {
namespace messages {
namespace fuse {

FileChildren::FileChildren(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    const auto &fileChildren =
        serverMessage->fuse_response().file_children().child_links();

    std::transform(fileChildren.begin(), fileChildren.end(),
        std::back_inserter(m_uuidsAndNames), [](const auto &child) {
            return std::make_tuple(child.uuid(), child.name());
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
