/**
 * @file tokenHandler.h
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TOKEN_HANDLER_H
#define ONECLIENT_TOKEN_HANDLER_H

#include "options.h"

#include <boost/filesystem/path.hpp>
#include <boost/optional.hpp>
#include <macaroons.hpp>

#include <chrono>
#include <string>

namespace one {
namespace client {
namespace auth {

constexpr std::chrono::minutes RESTRICTED_MACAROON_EXPIRATION{15};
constexpr std::chrono::minutes RESTRICTED_MACAROON_REFRESH{5};

/**
 * The @c TokenHandler class is responsible for retrieving and manipulating an
 * authorization macaroon, either from file or from user interaction.
 */
class TokenHandler {
public:
    /**
     * Constructor.
     * Schedules a refresh task for a restricted macaroon.
     * @param options An instance of options for token retrieval.
     * @param userDataDir Directory where user's application data is saved.
     * @param providerId ID of the provider who will be a recipient of
     * restricted tokens.
     */
    TokenHandler(Options &options, boost::filesystem::path userDataDir,
        std::string providerId);

    /**
     * @return A provider- and time-restricted token.
     */
    std::string restrictedToken() const;

    /**
     * Refreshes a restricted token saved in this handler.
     */
    std::string refreshRestrictedToken();

    /**
     * Tries to deserialize token assuming it is in base62 format.
     * If it fails, tries again assuming base64 format.
     * @param token Token to deserialize.
     * @return Deserialized macaroon.
     */
    macaroons::Macaroon deserialize(std::string token) const;

    /**
     * Decodes token in base62 format to base64 format.
     * @param token62 Token to decode.
     * @return Decoded token in base64 format.
     */
    static std::string decode62(std::string token62);

    /**
     * Encodes token in base64 format to base62 format.
     * @param token64 Token to encode.
     * @return Encoded token in base62 format.
     */
    static std::string encode62(std::string token64);

    /**
     * Removes file where token is cached.
     */
    void removeTokenFile() const;

private:
    macaroons::Macaroon retrieveToken() const;
    boost::optional<macaroons::Macaroon> readTokenFromFile() const;
    boost::optional<macaroons::Macaroon> getTokenFromOptions() const;
    macaroons::Macaroon getTokenFromUser() const;
    boost::filesystem::path tokenFilePath() const;
    void persistMacaroon(macaroons::Macaroon) const;

    Options &m_options;

    boost::filesystem::path m_userDataDir;
    std::string m_providerId;

    macaroons::Macaroon m_macaroon;
    macaroons::Macaroon m_restrictedMacaroon;
};

} // namespace auth
} // namespace client
} // namespace one

#endif // ONECLIENT_TOKEN_HANDLER_H
