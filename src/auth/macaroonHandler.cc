/**
 * @file macaroonHandler.cc
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "auth/macaroonHandler.h"

#include "auth/authException.h"
#include "context.h"
#include "helpers/logging.h"

#include <boost/bimap.hpp>
#include <boost/filesystem.hpp>
#include <boost/filesystem/fstream.hpp>
#include <boost/optional.hpp>

#include <cerrno>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <iostream>
#include <sys/stat.h>
#include <sys/types.h>
#include <system_error>

namespace {

macaroons::Macaroon restrictMacaroon(
    const macaroons::Macaroon &macaroon, const std::string & /*providerId*/)
{
    auto expiration = std::chrono::system_clock::now() +
        one::client::auth::RESTRICTED_MACAROON_EXPIRATION;

    auto expirationSinceEpoch =
        std::chrono::system_clock::to_time_t(expiration);

    return macaroon.addFirstPartyCaveat(
        "time < " + std::to_string(expirationSinceEpoch));
}

} // namespace

namespace one {
namespace client {
namespace auth {

MacaroonHandler::MacaroonHandler(options::Options &options,
    boost::filesystem::path userDataDir, std::string providerId)
    : m_options{options}
    , m_userDataDir{userDataDir}
    , m_providerId{std::move(providerId)}
    , m_macaroon{retrieveMacaroon()}
    , m_restrictedMacaroon{restrictMacaroon(m_macaroon, m_providerId)}
{
}

std::string MacaroonHandler::refreshRestrictedMacaroon()
{
    LOG_FCALL();

    m_restrictedMacaroon = restrictMacaroon(m_macaroon, m_providerId);
    return restrictedMacaroon();
}

std::string MacaroonHandler::restrictedMacaroon() const
{
    return encode62(m_restrictedMacaroon.serialize());
}

macaroons::Macaroon MacaroonHandler::retrieveMacaroon() const
{
    LOG_FCALL();

    if (auto macaroon = getMacaroonFromOptions()) {
        persistMacaroon(macaroon.get());
        return macaroon.get();
    }

    if (auto macaroon = readMacaroonFromFile()) {
        LOG(INFO) << "Retrieved access token from file " << macaroonFilePath();
        return macaroon.get();
    }

    try {
        auto macaroon = getMacaroonFromUser();
        persistMacaroon(macaroon);
        return macaroon;
    }
    catch (const std::exception &e) {
        LOG(ERROR) << "Failed to retrieve user's access token: " << e.what();
        throw BadAccess{"Invalid access token"};
    }
}

boost::optional<macaroons::Macaroon>
MacaroonHandler::readMacaroonFromFile() const
{
    LOG_FCALL();

    std::string macaroon;

    boost::filesystem::ifstream stream{macaroonFilePath()};
    stream >> macaroon;
    if (stream.fail() || stream.bad() || stream.eof()) {
        LOG(INFO) << "No cached access token found at: " << macaroonFilePath();
        return {};
    }

    try {
        return deserialize(macaroon);
    }
    catch (const macaroons::exception::Exception &e) {
        LOG(ERROR) << "Failed to parse access token retrieved from file "
                   << macaroonFilePath() << ": " << e.what();

        throw;
    }
}

boost::optional<macaroons::Macaroon>
MacaroonHandler::getMacaroonFromOptions() const
{
    LOG_FCALL();

    const auto &macaroon = m_options.getAccessToken();
    if (!macaroon)
        return {};

    try {
        return deserialize(macaroon.get());
    }
    catch (const macaroons::exception::Exception &e) {
        LOG(ERROR) << "Failed to parse access token passed on command line: "
                   << e.what();

        throw;
    }
}

macaroons::Macaroon MacaroonHandler::getMacaroonFromUser() const
{
    LOG_FCALL();

    std::string macaroon;
    std::cout << "Paste access token: ";

    auto prevExceptions = std::cin.exceptions();
    std::cin.exceptions(
        std::ios::failbit | std::ios::badbit | std::ios::eofbit);
    std::cin >> macaroon;
    std::cin.exceptions(prevExceptions);

    return deserialize(macaroon);
}

void MacaroonHandler::persistMacaroon(macaroons::Macaroon macaroon) const
{
    LOG_FCALL();

    try {
        boost::filesystem::ofstream stream{macaroonFilePath()};
        stream.exceptions(
            std::ios::failbit | std::ios::badbit | std::ios::eofbit);
        stream << macaroon.serialize() << std::endl;
        LOG(INFO) << "Saved authorization details to " << macaroonFilePath();

        const auto kMacaroonFilePermissions = 0600;

        if (chmod(macaroonFilePath().c_str(), kMacaroonFilePermissions) != 0) {
            const auto err = errno;
            LOG(ERROR) << "Failed to set file permissions on "
                       << macaroonFilePath() << ": " << strerror(err);
        }
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Failed to save authorization details to "
                     << macaroonFilePath() << " - " << e.what();
    }
}

boost::filesystem::path MacaroonHandler::macaroonFilePath() const
{
    return m_userDataDir / "macaroon";
}

macaroons::Macaroon MacaroonHandler::deserialize(std::string macaroon) const
{
    LOG_FCALL() << LOG_FARG(macaroon);

    try {
        return macaroons::Macaroon::deserialize(decode62(macaroon));
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Failed to deserialize access token as base62: "
                     << e.what() << ", trying to deserialize as base64";

        return macaroons::Macaroon::deserialize(macaroon);
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

} // namespace

std::string MacaroonHandler::decode62(std::string macaroon62)
{
    LOG_FCALL() << LOG_FARG(macaroon62);

    std::string macaroon64;
    macaroon64.reserve(macaroon62.size());

    for (auto it = macaroon62.begin(); it != macaroon62.end(); ++it) {
        if (*it == '0') {
            ++it;
            if (it == macaroon62.end())
                throw AuthException{"Unable to decode access token."};

            auto searchResult = coding.right.find(std::string{'0', *it});
            if (searchResult == coding.right.end())
                throw AuthException{"Unable to decode access token."};

            macaroon64 += searchResult->second;
        }
        else {
            macaroon64 += *it;
        }
    }

    return macaroon64;
}

std::string MacaroonHandler::encode62(std::string macaroon64)
{
    LOG_FCALL() << LOG_FARG(macaroon64);

    std::string macaroon62;
    macaroon62.reserve(macaroon64.size());

    for (auto c : macaroon64) {
        auto searchResult = coding.left.find(c);

        if (searchResult == coding.left.end()) {
            macaroon62 += c;
        }
        else {
            macaroon62 += searchResult->second;
        }
    }

    return macaroon62;
}

void MacaroonHandler::removeMacaroonFile() const
{
    if (!boost::filesystem::remove(macaroonFilePath())) {
        LOG(WARNING) << "Failed to remove access token file '"
                     << macaroonFilePath() << "'";
    }
}

} // namespace auth
} // namespace client
} // namespace one
