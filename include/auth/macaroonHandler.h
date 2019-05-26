/**
 * @file macaroonHandler.h
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MACAROON_HANDLER_H
#define ONECLIENT_MACAROON_HANDLER_H

#include "options/options.h"

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
 * The @c MacaroonHandler class is responsible for retrieving and manipulating
 * an authorization macaroon, either from file or from user interaction.
 */
class MacaroonHandler {
public:
    /**
     * Constructor.
     * Schedules a refresh task for a restricted macaroon.
     * @param options An instance of @c options::Options for macaroon retrieval.
     * @param userDataDir Directory where user's application data is saved.
     * @param providerId ID of the provider who will be a recipient of
     * restricted macaroons.
     */
    MacaroonHandler(options::Options &options,
        boost::filesystem::path userDataDir, std::string providerId);

    /**
     * @return A provider- and time-restricted macaroon.
     */
    std::string restrictedMacaroon() const;

    /**
     * Refreshes a restricted macaroon saved in this handler.
     */
    std::string refreshRestrictedMacaroon();

    /**
     * Tries to deserialize macaroon assuming it is in base62 format.
     * If it fails, tries again assuming base64 format.
     * @param macaron Macaroon to deserialize.
     * @return Deserialized macaroon.
     */
    macaroons::Macaroon deserialize(std::string macaroon) const;

    /**
     * Decodes macaroon in base62 format to base64 format.
     * @param macaroon62 Macaroon to decode.
     * @return Decoded macaroon in base64 format.
     */
    static std::string decode62(std::string macaroon62);

    /**
     * Encodes macaroon in base64 format to base62 format.
     * @param macaroon 64 Macaroon to encode.
     * @return Encoded macaroon in base62 format.
     */
    static std::string encode62(std::string macaroon64);

    /**
     * Removes file where macaroon is cached.
     */
    void removeMacaroonFile() const;

private:
    macaroons::Macaroon retrieveMacaroon() const;
    boost::optional<macaroons::Macaroon> readMacaroonFromFile() const;
    boost::optional<macaroons::Macaroon> getMacaroonFromOptions() const;
    macaroons::Macaroon getMacaroonFromUser() const;
    boost::filesystem::path macaroonFilePath() const;
    void persistMacaroon(macaroons::Macaroon) const;

    options::Options &m_options;

    boost::filesystem::path m_userDataDir;
    std::string m_providerId;

    macaroons::Macaroon m_macaroon;
    macaroons::Macaroon m_restrictedMacaroon;
};

} // namespace auth
} // namespace client
} // namespace one

#endif // ONECLIENT_MACAROON_HANDLER_H
