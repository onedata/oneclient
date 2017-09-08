/**
 * @file getFileChildrenAttrs.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getFileChildrenAttrs.h"

#include "messages.pb.h"

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

std::string GetFileChildrenAttrs::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetFileChildrenAttrs', uuid: " << m_contextGuid
           << ", offset: " << m_offset << ", size: " << m_size;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage>
GetFileChildrenAttrs::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto gfc = msg->mutable_fuse_request()
                   ->mutable_file_request()
                   ->mutable_get_file_children_attrs();

    gfc->set_offset(m_offset);
    gfc->set_size(m_size);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
