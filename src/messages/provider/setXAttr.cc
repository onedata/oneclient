/**
 * @file setXAttr.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "setXAttr.h"

#include "messages.pb.h"

#include <cassert>
#include <iostream>
#include <sstream>

namespace one {
namespace messages {
namespace provider {

SetXAttr::SetXAttr(folly::fbstring uuid, folly::fbstring name,
    folly::fbstring value, bool create, bool replace)
    : ProviderRequest{uuid.toStdString()}
    , m_name(name)
    , m_value(value)
    , m_create(create)
    , m_replace(replace)
{
}

std::string SetXAttr::toString() const
{
    std::stringstream stream;
    stream << "type: 'SetXAttr', uuid: " << m_contextGuid << ", "
           << " name: " << m_name << ", valueSize: " << m_value.size()
           << " create:" << m_create << ", replace: " << m_replace;

    return stream.str();
}

std::unique_ptr<ProtocolClientMessage> SetXAttr::serializeAndDestroy()
{
    auto msg = ProviderRequest::serializeAndDestroy();
    msg->mutable_provider_request()
        ->mutable_set_xattr()
        ->mutable_xattr()
        ->set_name(m_name.toStdString());

    msg->mutable_provider_request()
        ->mutable_set_xattr()
        ->mutable_xattr()
        ->set_value(m_value.toStdString());

    return msg;
}

} // namespace provider
} // namespace messages
} // namespace one
