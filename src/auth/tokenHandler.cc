/**
 * @file tokenHandler.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/tokenHandler.h"

#include "context.h"
#include "logging.h"
#include "auth/authException.h"

#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>

#include <fstream>
#include <iostream>
#include <system_error>

using namespace std::literals::chrono_literals;

namespace one {
namespace client {
namespace auth {

TokenHandler::TokenHandler(boost::filesystem::path userDataDir)
    : m_userDataDir{std::move(userDataDir)}
    , m_macaroon{retrieveToken()}
{
}

std::string TokenHandler::restrictedToken(const std::string &providerId) const
{
    auto expiration =
        std::chrono::system_clock::now() + RESTRICTED_MACAROON_EXPIRATION;

    auto expirationSinceEpoch =
        std::chrono::system_clock::to_time_t(expiration);

    return m_macaroon.addFirstPartyCaveat("providerId = " + providerId)
        .addFirstPartyCaveat("time < " + std::to_string(expirationSinceEpoch))
        .serialize();
}

macaroons::Macaroon TokenHandler::retrieveToken() const
{
    try {
        if (auto macaroon = readTokenFromFile())
            return macaroon.get();

        return getTokenFromUser();
    }
    catch (const macaroons::exception::Exception &e) {
        LOG(ERROR) << e.what();
        throw BadAccess{"invalid authorization token"};
    }
}

boost::optional<macaroons::Macaroon> TokenHandler::readTokenFromFile() const
{
    const auto accessTokenFile = tokenFilePath();

    boost::system::error_code ec;
    const auto exists = boost::filesystem::exists(accessTokenFile, ec);
    if (ec || !exists) {
        LOG(INFO) << "No previously saved authorization details exist under "
                     "path " << accessTokenFile;
        return {};
    }

    std::string token;

    try {
        boost::filesystem::ifstream stream{accessTokenFile};
        stream.exceptions(std::ios::failbit);
        stream >> token;
    }
    catch (const std::system_error &e) {
        LOG(WARNING) << "Failed to retrieve authorization details from "
                     << accessTokenFile << " - " << e.code().message();
        return {};
    }

    try {
        auto macaroon = macaroons::Macaroon::deserialize(token);
        return macaroon;
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Exception while deserializing saved macaroon - "
                     << e.what();
        return {};
    }
}

macaroons::Macaroon TokenHandler::getTokenFromUser() const
{
    const auto accessTokenFile = tokenFilePath();
    std::cout << "Authorization token: ";
    std::string token;
    std::cin >> token;

    auto macaroon = macaroons::Macaroon::deserialize(token);

    try {
        boost::filesystem::ofstream stream{accessTokenFile};
        stream.exceptions(std::ios::failbit);
        stream << token;
        LOG(INFO) << "Saved authorization details to " << accessTokenFile;
    }
    catch (const std::system_error &e) {
        LOG(WARNING) << "Failed to save authorization details to "
                     << accessTokenFile << " - " << e.code().message();
    }

    return macaroon;
}

boost::filesystem::path TokenHandler::tokenFilePath() const
{
    return m_userDataDir / "token";
}

} // namespace auth
} // namespace client
} // namespace one
