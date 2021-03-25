/**
 * @file makeLink.cc
 * @author Bartek Kryza
 * @copyright (C) 2021 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "makeLink.h"

#include "messages.pb.h"
#include "spdlog/spdlog.h"

#include <memory>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

MakeLink::MakeLink(
    folly::fbstring uuid, folly::fbstring parentUuid, folly::fbstring name)
    : FileRequest{uuid.toStdString()}
    , m_parentUuid{std::move(parentUuid)}
    , m_name{std::move(name)}
{
}

std::string MakeLink::toString() const
{
    return fmt::format(
        "type: 'MakeLink', uuid: '{}', name: '{}', parentUUID: '{}'",
        m_contextGuid, m_name, m_parentUuid);
}

std::unique_ptr<ProtocolClientMessage> MakeLink::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    auto mf = msg->mutable_fuse_request()
                  ->mutable_file_request()
                  ->mutable_make_link();

    mf->set_target_parent_uuid(m_parentUuid.toStdString());
    mf->set_target_name(m_name.toStdString());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
