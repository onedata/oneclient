/**
 * @file tokenHandler.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/tokenHandler.h"

#include "auth/authException.h"
#include "context.h"
#include "logging.h"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/optional.hpp>

#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <cstdlib>
#include <fstream>
#include <ios>
#include <iostream>
#include <system_error>

namespace {

macaroons::Macaroon restrictMacaroon(
    const macaroons::Macaroon &macaroon, const std::string &providerId)
{
    auto expiration = std::chrono::system_clock::now() +
        one::client::auth::RESTRICTED_MACAROON_EXPIRATION;

    auto expirationSinceEpoch =
        std::chrono::system_clock::to_time_t(expiration);

    return macaroon /*.addFirstPartyCaveat("providerId = " + providerId)*/
        .addFirstPartyCaveat("time < " + std::to_string(expirationSinceEpoch));
}

} // namespace

namespace one {
namespace client {
namespace auth {

TokenHandler::TokenHandler(Options &options,
    boost::filesystem::path userDataDir, std::string providerId)
    : m_options{options}
    , m_userDataDir{std::move(userDataDir)}
    , m_providerId{std::move(providerId)}
    , m_macaroon{retrieveToken()}
    , m_restrictedMacaroon{restrictMacaroon(m_macaroon, m_providerId)}
{
}

std::string TokenHandler::refreshRestrictedToken()
{
    m_restrictedMacaroon = restrictMacaroon(m_macaroon, m_providerId);
    return restrictedToken();
}

std::string TokenHandler::restrictedToken() const
{
    return m_restrictedMacaroon.serialize();
}

macaroons::Macaroon TokenHandler::retrieveToken() const
{
    if (auto macaroon = getTokenFromOptions()) {
        persistMacaroon(macaroon.get());
        return macaroon.get();
    }

    if (auto macaroon = readTokenFromFile()) {
        LOG(INFO) << "Retrieved token from " << tokenFilePath();
        return macaroon.get();
    }

    try {
        auto macaroon = getTokenFromUser();
        persistMacaroon(macaroon);
        return macaroon;
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Failed to retrieve token from user: " << e.what();
        throw BadAccess{"invalid authorization token"};
    }
}

boost::optional<macaroons::Macaroon> TokenHandler::readTokenFromFile() const
{
    std::string token;

    boost::filesystem::ifstream stream{tokenFilePath()};
    stream >> token;
    if (stream.fail() || stream.bad() || stream.eof()) {
        LOG(WARNING) << "Failed to retrieve token from file "
                     << tokenFilePath();
        return {};
    }

    try {
        return macaroons::Macaroon::deserialize(token);
    }
    catch (const macaroons::exception::Exception &e) {
        LOG(WARNING) << "Failed to parse macaroon retrieved from file "
                     << tokenFilePath() << ": " << e.what();

        return {};
    }
}

boost::optional<macaroons::Macaroon> TokenHandler::getTokenFromOptions() const
{
    if (!m_options.has_authorization_token())
        return {};

    try {
        auto macaroon = macaroons::Macaroon::deserialize(
            m_options.get_authorization_token());

        return macaroon;
    }
    catch (const macaroons::exception::Exception &e) {
        LOG(WARNING) << "Failed to parse macaroon retrieved from options: "
                     << e.what();

        return {};
    }
}

macaroons::Macaroon TokenHandler::getTokenFromUser() const
{
    std::string token;
    std::cout << "Authorization token: ";

    auto prevExceptions = std::cin.exceptions();
    std::cin.exceptions(
        std::ios::failbit | std::ios::badbit | std::ios::eofbit);
    std::cin >> token;
    std::cin.exceptions(prevExceptions);

    return macaroons::Macaroon::deserialize(token);
}

void TokenHandler::persistMacaroon(macaroons::Macaroon macaroon) const
{
    try {
        boost::filesystem::ofstream stream{tokenFilePath()};
        stream.exceptions(
            std::ios::failbit | std::ios::badbit | std::ios::eofbit);
        stream << macaroon.serialize() << std::endl;
        LOG(INFO) << "Saved authorization details to " << tokenFilePath();

        if (chmod(tokenFilePath().c_str(), 0600) != 0) {
            const auto err = errno;
            LOG(ERROR) << "Failed to set file permissions on "
                       << tokenFilePath() << ": " << strerror(err);
        }
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Failed to save authorization details to "
                     << tokenFilePath() << " - " << e.what();
    }
}

boost::filesystem::path TokenHandler::tokenFilePath() const
{
    return m_userDataDir / "token";
}

} // namespace auth
} // namespace client
} // namespace one
