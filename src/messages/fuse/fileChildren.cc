/**
 * @file fileChildren.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fileChildren.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

FileChildren::FileChildren(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : m_uuids{serverMessage->fuse_response().file_children().uuids().begin(),
          serverMessage->fuse_response().file_children().uuids().end()}
{
}

const std::vector<std::string> &FileChildren::uuids() const { return m_uuids; }

std::string FileChildren::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileChildren', uuids: [";

    for (const auto &uuid : m_uuids)
        stream << uuid << " ";

    stream << "]";

    return stream.str();
}

} // namespace fuse
} // namespace messages
} // namespace one
