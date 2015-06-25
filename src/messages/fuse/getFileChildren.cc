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
    : m_uuid(std::move(uuid))
{
}

GetFileChildren::GetFileChildren(std::string uuid, std::size_t offset, std::size_t size)
    : m_uuid{std::move(uuid)}
    , m_offset{offset}
    , m_size{size}
{
}

std::string GetFileChildren::toString() const
{
    std::stringstream stream;

    stream << "type: 'GetFileChildren', uuid: " << m_uuid;
    if (m_offset)
        stream << ", offset: " << m_offset.get();
    if (m_size)
        stream << ", size: " << m_size.get();

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileChildren::serialize() const
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto gfc = msg->mutable_fuse_request()->mutable_get_file_children();

    gfc->set_uuid(m_uuid);
    if (m_offset)
        gfc->set_offset(m_offset.get());
    if (m_size)
        gfc->set_size(m_size.get());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
