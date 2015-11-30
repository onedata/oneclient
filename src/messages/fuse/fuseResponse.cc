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

    auto &statusMsg = serverMessage->fuse_response().status();

    std::error_code code;
    boost::optional<std::string> description;
    std::tie(code, description) = Status::translate(statusMsg);

    if (code) {
        if (description)
            throw std::system_error{code, description.get()};

        throw std::system_error{code};
    }
}

std::string FuseResponse::toString() const { return {}; }

} // namespace fuse
} // namespace messages
} // namespace one
