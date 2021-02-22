/**
 * @file makeSymLink.cc
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "makeSymLink.h"

#include "messages.pb.h"
#include "spdlog/spdlog.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

MakeSymLink::MakeSymLink(
    folly::fbstring parentUuid, folly::fbstring name, folly::fbstring link)
    : FileRequest{parentUuid.toStdString()}
    , m_name{std::move(name)}
    , m_link{std::move(link)}
{
}

std::string MakeSymLink::toString() const
{
    return fmt::format(
        "type: 'MakeSymLink', parentUuid: '{}', name: '{}', link: '{}'",
        m_contextGuid, m_name, m_link);
}

std::unique_ptr<ProtocolClientMessage> MakeSymLink::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto mf = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_make_symlink();

    mf->set_target_name(m_name.toStdString());
    mf->set_link(m_link.toStdString());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
