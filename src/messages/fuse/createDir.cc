/**
 * @file createDir.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "createDir.h"

#include "messages.pb.h"

#include <sstream>

namespace one {
namespace messages {
namespace fuse {

CreateDir::CreateDir(std::string parentUUID, std::string name, mode_t mode)
    : m_parentUUID{std::move(parentUUID)}
    , m_name{std::move(name)}
    , m_mode{mode}
{
}

std::string CreateDir::toString() const
{
    std::stringstream stream;
    stream << "type: 'CreateDir', parentUUID: " << m_parentUUID
           << ", name: " << m_name << ", mode: " << std::oct << m_mode;
    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> CreateDir::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto cd = msg->mutable_fuse_request()->mutable_create_dir();

    cd->mutable_parent_uuid()->swap(m_parentUUID);
    cd->mutable_name()->swap(m_name);
    cd->set_mode(m_mode);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
