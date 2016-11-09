/**
 * @file getChildAttr.cc
 * @author Konrad Zemek
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "getChildAttr.h"

#include "messages.pb.h"

#include <cassert>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

GetChildAttr::GetChildAttr(folly::fbstring uuid, folly::fbstring name)
    : FileRequest{uuid.toStdString()}
    , m_name{std::move(name)}
{
}

std::string GetChildAttr::toString() const
{
    return "type: 'GetChildAttr', uuid: '" + m_contextGuid + "', name: '" +
        m_name.toStdString() + '"';
}

std::unique_ptr<ProtocolClientMessage> GetChildAttr::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_get_child_attr()
        ->set_name(m_name.toStdString());

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
