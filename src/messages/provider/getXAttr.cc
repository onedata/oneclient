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
namespace provider {

GetXAttr::GetXAttr(folly::fbstring uuid, folly::fbstring name)
    : ProviderRequest{uuid.toStdString()}
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
    auto msg = ProviderRequest::serializeAndDestroy();
    msg->mutable_provider_request()->mutable_get_xattr()->set_name(
        m_name.toStdString());
    msg->mutable_provider_request()->mutable_get_xattr()->set_inherited(false);

    return msg;
}

} // namespace provider
} // namespace messages
} // namespace one
