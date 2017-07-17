/**
 * @file listXAttr.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "listXAttr.h"

#include "messages.pb.h"

#include <cassert>
#include <iostream>
#include <sstream>

namespace one {
namespace messages {
namespace fuse {

ListXAttr::ListXAttr(folly::fbstring uuid)
    : FileRequest{uuid.toStdString()}
{
}

std::string ListXAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'ListXAttr', uuid: " << m_contextGuid;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> ListXAttr::serializeAndDestroy()
{
    auto msg = FileRequest::serializeAndDestroy();
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_list_xattr()
        ->set_inherited(false);
    msg->mutable_fuse_request()
        ->mutable_file_request()
        ->mutable_list_xattr()
        ->set_show_internal(true);

    return msg;
}

} // namespace fuse
} // namespace messages
} // namespace one
