/**
 * @file getXAttr.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getXAttr.h"

#include "messages.pb.h"

#include <cassert>
#include <iostream>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetXAttr::GetXAttr(folly::fbstring uuid, folly::fbstring name)
    : FileRequest{uuid.toStdString()}
    , m_name(name)
{
}

std::string GetXAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'GetXAttr', uuid: " << m_contextGuid
           << ", name: " << m_name;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> GetXAttr::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_get_xattr()
        ->set_name(m_name.toStdString());
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_get_xattr()
        ->set_inherited(false);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
