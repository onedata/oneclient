/**
 * @file getFileAttr.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getFileAttr.h"

#include "messages.pb.h"

#include <cassert>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetFileAttr::GetFileAttr(std::string uuid)
    : m_uuid{std::move(uuid)}
{
}

GetFileAttr::GetFileAttr(boost::filesystem::path path)
    : m_path{std::move(path)}
{
}

std::string GetFileAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetFileAttr', ";

    if (m_uuid)
        stream << "uuid: " << m_uuid.get();
    else
        stream << "path: " << m_path.get();

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetFileAttr::serialize() const
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto gfa = msg->mutable_fuse_request()->mutable_get_file_attr();

    assert(m_uuid || m_path);

    if (m_uuid)
        gfa->set_uuid(m_uuid.get());
    else
        gfa->set_path(m_path.get().string());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
