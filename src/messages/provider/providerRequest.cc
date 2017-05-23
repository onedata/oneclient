/**
 * @file providerRequest.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "providerRequest.h"

#include "messages.pb.h"

#include <string>

namespace one {
namespace messages {
namespace provider {

ProviderRequest::ProviderRequest(std::string contextGuid)
    : m_contextGuid{std::move(contextGuid)}
{
}

std::unique_ptr<one::clproto::ClientMessage>
ProviderRequest::serializeAndDestroy()
{
    auto msg = std::make_unique<one::clproto::ClientMessage>();
    msg->mutable_provider_request()->set_context_guid(m_contextGuid);

    return msg;
}

} // namespace provider
} // namespace messages
} // namespace one
