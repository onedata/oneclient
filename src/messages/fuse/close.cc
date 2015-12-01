/**
 * @file close.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "close.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

Close::Close(std::string uuid)
    : m_uuid{std::move(uuid)}
{
}

std::string Close::toString() const
{
    std::stringstream stream;
    stream << "type: 'Close', uuid: " << m_uuid;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> Close::serialize() const
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto cm = msg->mutable_fuse_request()->mutable_close();
    cm->set_uuid(m_uuid);
    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
