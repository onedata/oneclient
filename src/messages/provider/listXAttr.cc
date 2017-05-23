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
namespace provider {

ListXAttr::ListXAttr(folly::fbstring uuid)
    : ProviderRequest{uuid.toStdString()}
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
    auto msg = ProviderRequest::serializeAndDestroy();
    msg->mutable_provider_request()->mutable_list_xattr()->set_inherited(false);
    msg->mutable_provider_request()->mutable_list_xattr()->set_show_internal(
        true);

    return msg;
}

} // namespace provider
} // namespace messages
} // namespace one
