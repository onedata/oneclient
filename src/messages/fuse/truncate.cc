/**
 * @file truncate.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "truncate.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

Truncate::Truncate(std::string uuid, const off_t size)
    : FileRequest{std::move(uuid)}
    , m_size{size}
{
}

std::string Truncate::toString() const
{
    std::stringstream stream;
    stream << "type: 'Truncate', uuid: " << m_contextGuid
           << "', size: " << m_size;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Truncate::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto cm =
        msg->mutable_fuse_request()->mutable_file_request()->mutable_truncate();

    cm->set_size(m_size);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
