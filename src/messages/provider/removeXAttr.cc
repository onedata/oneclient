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
namespace provider {

RemoveXAttr::RemoveXAttr(folly::fbstring uuid, folly::fbstring name)
    : ProviderRequest{uuid.toStdString()}
    , m_name(name)
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
    auto msg = ProviderRequest::serializeAndDestroy();
    msg->mutable_provider_request()->mutable_remove_xattr()->set_name(
        m_name.toStdString());

    return msg;
}

} // namespace provider
} // namespace messages
} // namespace one
