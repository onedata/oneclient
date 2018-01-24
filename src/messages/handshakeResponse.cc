/**
 * @file handshakeResponse.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "messages/handshakeResponse.h"
#include "errors/handshakeErrors.h"

#include <sstream>

namespace one {
namespace messages {

HandshakeResponse::HandshakeResponse(
    std::unique_ptr<ProtocolServerMessage> serverMessage)
{
    auto &msg = serverMessage->handshake_response();
    m_status = translateStatus(msg);
}

bool HandshakeResponse::isTokenError() const
{
    using errors::handshake::ErrorCode;

    return m_status != ErrorCode::ok &&
        m_status != ErrorCode::internal_server_error;
}

std::error_code HandshakeResponse::status() const
{
    return errors::handshake::makeErrorCode(m_status);
}

std::string HandshakeResponse::toString() const
{
    std::stringstream stream;
    stream << "type: 'HandshakeResponse', status: '" << status().message()
           << "'";
    return stream.str();
}

errors::handshake::ErrorCode HandshakeResponse::translateStatus(
    const one::clproto::HandshakeResponse &msg)
{
    using clproto::HandshakeStatus;
    using errors::handshake::ErrorCode;

    switch (msg.status()) {
        case HandshakeStatus::OK:
            return ErrorCode::ok;
        case HandshakeStatus::TOKEN_NOT_FOUND:
            return ErrorCode::token_not_found;
        case HandshakeStatus::TOKEN_EXPIRED:
            return ErrorCode::token_expired;
        case HandshakeStatus::INVALID_TOKEN:
            return ErrorCode::invalid_token;
        case HandshakeStatus::INVALID_METHOD:
            return ErrorCode::invalid_method;
        case HandshakeStatus::ROOT_RESOURCE_NOT_FOUND:
            return ErrorCode::root_resource_not_found;
        case HandshakeStatus::INVALID_PROVIDER:
            return ErrorCode::invalid_provider;
        case HandshakeStatus::BAD_SIGNATURE_FOR_MACAROON:
            return ErrorCode::bad_signature_for_macaroon;
        case HandshakeStatus::FAILED_TO_DESCRYPT_CAVEAT:
            return ErrorCode::failed_to_decrypt_caveat;
        case HandshakeStatus::NO_DISCHARGE_MACAROON_FOR_CAVEAT:
            return ErrorCode::no_discharge_macaroon_for_caveat;
        case HandshakeStatus::INCOMPATIBLE_VERSION:
            return ErrorCode::incompatible_version;
        default:
            return ErrorCode::internal_server_error;
    }
}

} // namespace messages
} // namespace one
