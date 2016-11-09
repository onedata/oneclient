/**
 * @file getFileChildren.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getFileChildren.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetFileChildren::GetFileChildren(
    const folly::fbstring &uuid, const off_t offset, const std::size_t size)
    : FileRequest{uuid.toStdString()}
    , m_offset{offset}
    , m_size{size}
{
}

std::string GetFileChildren::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetFileChildren', uuid: " << m_contextGuid
           << ", offset: " << m_offset << ", size: " << m_size;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileChildren::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto gfc = msg->mutable_fuse_request()
                   ->mutable_file_request()
                   ->mutable_get_file_children();

    gfc->set_offset(m_offset);
    gfc->set_size(m_size);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
