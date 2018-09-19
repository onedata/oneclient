/**
 * @file removeXAttr.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "removeXAttr.h"

#include "messages.pb.h"

#include <cassert>
#include <iostream>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

RemoveXAttr::RemoveXAttr(folly::fbstring uuid, folly::fbstring name)
    : FileRequest{uuid.toStdString()}
    , m_name{std::move(name)}
{
}

std::string RemoveXAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetXAttr', uuid: " << m_contextGuid;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> RemoveXAttr::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_remove_xattr()
        ->set_name(m_name.toStdString());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
