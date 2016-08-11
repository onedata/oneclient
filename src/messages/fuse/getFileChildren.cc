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

GetFileChildren::GetFileChildren(std::string uuid)
    : FileRequest(std::move(uuid))
{
}

GetFileChildren::GetFileChildren(
    std::string uuid, off_t offset, std::size_t size)
    : FileRequest{std::move(uuid)}
    , m_offset{offset}
    , m_size{size}
{
}

std::string GetFileChildren::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetFileChildren', uuid: " << m_contextGuid;
    if (m_offset)
        stream << ", offset: " << m_offset.get();
    if (m_size)
        stream << ", size: " << m_size.get();

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileChildren::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto gfc = msg->mutable_fuse_request()
                   ->mutable_file_request()
                   ->mutable_get_file_children();

    if (m_offset)
        gfc->set_offset(m_offset.get());
    if (m_size)
        gfc->set_size(m_size.get());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
