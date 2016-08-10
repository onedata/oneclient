/**
 * @file handshakeErrors.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_ERRORS_HANDSHAKE_ERRORS_H
#define HELPERS_ERRORS_HANDSHAKE_ERRORS_H

#include <system_error>

namespace one {
namespace errors {
namespace handshake {

/**
 * The ErrorCode class enumerates possible handshake errors.
 */
enum class ErrorCode {
    ok,
    token_expired,
    token_not_found,
    invalid_token,
    invalid_method,
    root_resource_not_found,
    invalid_provider,
    bad_signature_for_macaroon,
    failed_to_decrypt_caveat,
    no_discharge_macaroon_for_caveat,
    internal_server_error
};

/**
 * The HandshakeErrorCategory class represents handshake error.
 */
class HandshakeErrorCategory : public std::error_category {
public:
    const char *name() const noexcept override;
    std::string message(int ev) const override;
};

/**
 * Translates handshake error code into standard library error code.
 * @param code an instance of @c ErrorCode
 * @return standard library error code instance
 */
std::error_code makeErrorCode(ErrorCode code);

} // namespace handshake
} // namespace errors
} // namespace one

namespace std {
template <>
struct is_error_code_enum<one::errors::handshake::ErrorCode>
    : public true_type {
};
} // namespace std

#endif // HELPERS_ERRORS_HANDSHAKE_ERRORS_H
