/**
 * @file tokenHandler.h
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_TOKEN_HANDLER_H
#define ONECLIENT_TOKEN_HANDLER_H

#include <boost/filesystem/path.hpp>
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
     * @param userDataDir Directory where user's application data is saved.
     * @param providerId ID of the provider who will be a recipient of
     * restricted tokens.
     */
    TokenHandler(boost::filesystem::path userDataDir, std::string providerId);

    /**
     * @return A provider- and time-restricted token.
     */
    std::string restrictedToken() const;

    /**
     * Refreshes a restricted token saved in this handler.
     */
    std::string refreshRestrictedToken();

private:
    macaroons::Macaroon retrieveToken() const;
    macaroons::Macaroon readTokenFromFile() const;
    macaroons::Macaroon getTokenFromUser() const;
    boost::filesystem::path tokenFilePath() const;

    boost::filesystem::path m_userDataDir;
    std::string m_providerId;

    macaroons::Macaroon m_macaroon;
    macaroons::Macaroon m_restrictedMacaroon;
};

} // namespace auth
} // namespace client
} // namespace one

#endif // ONECLIENT_TOKEN_HANDLER_H
