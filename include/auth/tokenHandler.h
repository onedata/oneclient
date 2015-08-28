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
#include <boost/optional.hpp>
#include <macaroons.hpp>

#include <chrono>
#include <string>

namespace one {
namespace client {
namespace auth {

constexpr std::chrono::minutes RESTRICTED_MACAROON_EXPIRATION{15};

/**
 * The @c TokenHandler class is responsible for retrieving and manipulating an
 * authorization macaroon, either from file or from user interaction.
 */
class TokenHandler {
public:
    /**
     * Constructor.
     * @param userDataDir Directory where user's application data is saved.
     */
    TokenHandler(boost::filesystem::path userDataDir);

    /**
     * Creates a new restricted token for a given provider.
     * @param providerId ID of the provider who is the recipient of the new
     * token.
     * @return The restricted token.
     */
    std::string restrictedToken(const std::string &providerId) const;

private:
    macaroons::Macaroon retrieveToken() const;
    boost::optional<macaroons::Macaroon> readTokenFromFile() const;
    macaroons::Macaroon getTokenFromUser() const;
    boost::filesystem::path tokenFilePath() const;

    boost::filesystem::path m_userDataDir;
    macaroons::Macaroon m_macaroon;
};

} // namespace auth
} // namespace client
} // namespace one

#endif // ONECLIENT_TOKEN_HANDLER_H
