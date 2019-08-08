/**
 * @file resolveGuid.cc
 * @author Mateusz Paciorek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "resolveGuid.h"

#include "messages.pb.h"

#include <cassert>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

ResolveGuid::ResolveGuid(boost::filesystem::path path)
    : m_path{std::move(path)}
{
}

std::string ResolveGuid::toString() const
{
    std::stringstream stream;
    stream << "type: 'ResolveGuid', path: " << m_path;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> ResolveGuid::serializeAndDestroy()
{
    auto msg = std::make_unique<ProtocolClientMessage>();
    auto rg = msg->mutable_fuse_request()->mutable_resolve_guid();

    rg->set_path(m_path.string());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
