/**
 * @file providerResponse.cc
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "providerResponse.h"

#include "messages.pb.h"
#include "messages/status.h"
#include <iostream>

namespace one {
namespace messages {
namespace provider {

ProviderResponse::ProviderResponse(
    const std::unique_ptr<ProtocolServerMessage> &serverMessage)
{
    if (!serverMessage->has_provider_response()) {
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "provider_response field missing"};
    }

    Status{*serverMessage->mutable_provider_response()->mutable_status()}
        .throwOnError();
}

std::string ProviderResponse::toString() const
{
    return {"type: 'ProviderResponse'"};
}

} // namespace provider
} // namespace messages
} // namespace one
