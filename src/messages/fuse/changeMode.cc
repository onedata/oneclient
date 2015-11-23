/**
 * @file changeMode.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "changeMode.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

ChangeMode::ChangeMode(std::string uuid, mode_t mode)
    : m_uuid{std::move(uuid)}
    , m_mode{mode}
{
}

std::string ChangeMode::toString() const
{
    std::stringstream stream;
    stream << "type: 'ChangeMode', uuid: " << m_uuid << ", mode: " << std::oct
           << m_mode;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> ChangeMode::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto cm = msg->mutable_fuse_request()->mutable_change_mode();

    cm->mutable_uuid()->swap(m_uuid);
    cm->set_mode(m_mode);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
