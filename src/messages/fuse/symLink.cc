/**
 * @file symLink.cc
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "symLink.h"

#include "messages.pb.h"
#include "spdlog/spdlog.h"

#include <sstream>
#include <system_error>

namespace one {
namespace messages {
namespace fuse {

SymLink::SymLink(std::unique_ptr<ProtocolServerMessage> serverMessage)
    : FuseResponse{serverMessage}
{
    if (!serverMessage->fuse_response().has_symlink()) {
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "symlink field missing"};
    }

    auto symlink = serverMessage->mutable_fuse_response()->mutable_symlink();

    m_link = symlink->link();
}

const std::string &SymLink::link() const { return m_link; }


std::string SymLink::toString() const
{
    return fmt::format("type: 'SymLink', link: '{}'", m_link);
}

} // namespace fuse
} // namespace messages
} // namespace one
