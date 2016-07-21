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

#include <boost/bimap.hpp>
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

TokenHandler::TokenHandler(
    boost::filesystem::path userDataDir, std::string providerId)
    : m_userDataDir{std::move(userDataDir)}
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
    return encode62(m_restrictedMacaroon.serialize());
}

macaroons::Macaroon TokenHandler::retrieveToken() const
{
    try {
        auto token = readTokenFromFile();
        if (token)
            return token.get();
    }
    catch (const macaroons::exception::Exception &e) {
        LOG(WARNING) << "Failed to parse macaroon retrieved from file "
                     << tokenFilePath() << ": " << e.what();
    }

    try {
        return getTokenFromUser();
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

    return deserialize(token);
}

macaroons::Macaroon TokenHandler::getTokenFromUser() const
{
    std::string token;

    if (auto tokenChr = std::getenv(AUTHORIZATION_TOKEN_ENV)) {
        token = tokenChr;
    }
    else {
        std::cout << "Authorization token: ";

        auto prevExceptions = std::cin.exceptions();
        std::cin.exceptions(
            std::ios::failbit | std::ios::badbit | std::ios::eofbit);
        std::cin >> token;
        std::cin.exceptions(prevExceptions);
    }

    auto macaroon = deserialize(token);

    try {
        boost::filesystem::ofstream stream{tokenFilePath()};
        stream.exceptions(
            std::ios::failbit | std::ios::badbit | std::ios::eofbit);
        stream << token << std::endl;
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

    return macaroon;
}

boost::filesystem::path TokenHandler::tokenFilePath() const
{
    return m_userDataDir / "token";
}

macaroons::Macaroon TokenHandler::deserialize(std::string token) const
{
    try {
        return macaroons::Macaroon::deserialize(decode62(token));
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Failed to deserialize token as base62: " << e.what()
                     << ", trying to deserialize as base64";

        return macaroons::Macaroon::deserialize(token);
    }
}

namespace {

using Coding = boost::bimap<char, std::string>;

Coding createCoding()
{
    const std::vector<Coding::value_type> pairs{{'0', "00"}, {'_', "01"},
        {'-', "02"}, {'/', "03"}, {'+', "04"}, {'=', "05"}};

    return {pairs.begin(), pairs.end()};
}

const Coding coding = createCoding();
}

std::string TokenHandler::decode62(std::string token62) const
{
    std::string token64;
    token64.reserve(token62.size());

    for (auto it = token62.begin(); it != token62.end(); ++it) {
        if (*it == '0') {
            ++it;
            if (it == token62.end())
                throw AuthException{"Unable to decode token."};

            auto searchResult = coding.right.find(std::string{'0', *it});
            if (searchResult == coding.right.end())
                throw AuthException{"Unable to decode token."};

            token64 += searchResult->second;
        }
        else {
            token64 += *it;
        }
    }

    return token64;
}

std::string TokenHandler::encode62(std::string token64) const
{
    std::string token62;
    token62.reserve(token64.size());

    for (auto c : token64) {
        auto searchResult = coding.left.find(c);

        if (searchResult == coding.left.end()) {
            token62 += c;
        }
        else {
            token62 += searchResult->second;
        }
    }

    return token62;
}

} // namespace auth
} // namespace client
} // namespace one
