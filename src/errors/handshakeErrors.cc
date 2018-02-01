/**
 * @file handshakeErrors.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "handshakeErrors.h"

#include <string>

namespace one {
namespace errors {
namespace handshake {

const char *HandshakeErrorCategory::name() const noexcept
{
    return "handshake";
}

std::string HandshakeErrorCategory::message(int ev) const
{
    switch (static_cast<ErrorCode>(ev)) {
        case ErrorCode::ok:
            return "ok";
        case ErrorCode::token_expired:
            return "token expired";
        case ErrorCode::token_not_found:
            return "token not found";
        case ErrorCode::invalid_token:
            return "invalid token";
        case ErrorCode::invalid_method:
            return "invalid method for the token";
        case ErrorCode::root_resource_not_found:
            return "required root resource not found in the token";
        case ErrorCode::invalid_provider:
            return "invalid provider";
        case ErrorCode::bad_signature_for_macaroon:
            return "bad token signature";
        case ErrorCode::failed_to_decrypt_caveat:
            return "failed to decrypt a token caveat";
        case ErrorCode::no_discharge_macaroon_for_caveat:
            return "discharge macaroon not found for a token caveat";
        case ErrorCode::incompatible_version:
            return "incompatible Oneprovider version";
        default:
            return "internal server error";
    }
}

std::error_code makeErrorCode(ErrorCode e)
{
    static HandshakeErrorCategory errorCategory{};
    return std::error_code(static_cast<int>(e), errorCategory);
}

} // namespace handshake
} // namespace errors
} // namespace one
