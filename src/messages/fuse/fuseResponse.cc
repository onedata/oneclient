/**
 * @file fuseResponse.cc
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fuseResponse.h"

#include "messages.pb.h"
#include "messages/status.h"

namespace one {
namespace messages {
namespace fuse {

FuseResponse::FuseResponse(
    const std::unique_ptr<ProtocolServerMessage> &serverMessage)
{
    if (!serverMessage->has_fuse_response())
        throw std::system_error{std::make_error_code(std::errc::protocol_error),
            "fuse_response field missing"};

    Status{*serverMessage->mutable_fuse_response()->mutable_status()}
        .throwOnError();
}

std::string FuseResponse::toString() const { return {"type: 'FuseResponse'"}; }

} // namespace fuse
} // namespace messages
} // namespace one
