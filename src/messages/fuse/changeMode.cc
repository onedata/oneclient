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
    : FileRequest{std::move(uuid)}
    , m_mode{mode}
{
}

std::string ChangeMode::toString() const
{
    std::stringstream stream;
    stream << "type: 'ChangeMode', "
           << "uuid: " << m_contextGuid << ", mode: " << std::oct
           << m_mode;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> ChangeMode::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto cm = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_change_mode();

    cm->set_mode(m_mode);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
