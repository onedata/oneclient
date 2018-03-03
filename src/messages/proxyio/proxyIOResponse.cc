/**
 * @file proxyIOResponse.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "proxyIOResponse.h"

#include "logging.h"
#include "messages.pb.h"
#include "messages/status.h"

#include <system_error>

namespace one {
namespace messages {
namespace proxyio {

ProxyIOResponse::ProxyIOResponse(
    const std::unique_ptr<ProtocolServerMessage> &serverMessage)
{
    if (!serverMessage->has_proxyio_response()) {
        LOG(ERROR) << "Invalid ServerMessage - missing proxyio_response field";
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "proxyio_response field missing"};
    }

    Status{*serverMessage->mutable_proxyio_response()->mutable_status()}
        .throwOnError();
}

} // namespace proxyio
} // namespace messages
} // namespace one
