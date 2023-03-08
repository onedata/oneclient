/**
 * @file macaroonHandler.h
 * @author Konrad Zemek
 * @copyright (C) 2014-2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MACAROON_HANDLER_H
#define ONECLIENT_MACAROON_HANDLER_H

#include "auth/authException.h"
#include "options/options.h"

#include <boost/filesystem/path.hpp>
#include <boost/optional.hpp>
#include <macaroons.hpp>

#include <chrono>
#include <string>

#include <cerrno>
#include <cstdlib>
#include <fstream>
#include <ios>
#include <iostream>
#include <sys/stat.h>
#include <sys/types.h>
#include <system_error>

namespace one {
namespace client {
namespace auth {

constexpr std::chrono::minutes RESTRICTED_MACAROON_EXPIRATION{20};
constexpr std::chrono::minutes RESTRICTED_MACAROON_REFRESH{10};

std::string decode62(std::string macaroon62);
std::string encode62(const std::string &macaroon64);
macaroons::Macaroon restrictMacaroon(const macaroons::Macaroon &macaroon);

/**
 * Tries to deserialize macaroon assuming it is in base62 format.
 * If it fails, tries again assuming base64 format.
 * @param macaron Macaroon to deserialize.
 * @return Deserialized macaroon.
 */
macaroons::Macaroon deserialize(const std::string &macaroon);

class MacaroonRetrievePolicyFromOptions {
public:
    MacaroonRetrievePolicyFromOptions(options::Options &options);

    macaroons::Macaroon retrieveMacaroon() const;

private:
    options::Options &m_options;
};

class MacaroonRetrievePolicyFromCLI {
public:
    MacaroonRetrievePolicyFromCLI(
        options::Options &options, boost::filesystem::path userDataDir);

    macaroons::Macaroon retrieveMacaroon() const;

private:
    boost::optional<macaroons::Macaroon> readMacaroonFromFile() const;

    boost::optional<macaroons::Macaroon> getMacaroonFromOptions() const;

    macaroons::Macaroon getMacaroonFromUser() const;

    boost::filesystem::path macaroonFilePath() const;

    options::Options &m_options;
    boost::filesystem::path m_userDataDir;
};

class MacaroonPersistPolicyNone {
public:
    void persistMacaroon(const macaroons::Macaroon &macaroon) { }
    void removeMacaroon() { }
};

class MacaroonPersistPolicyFile {
public:
    MacaroonPersistPolicyFile(boost::filesystem::path userDataDir);

    void persistMacaroon(const macaroons::Macaroon &macaroon);

    void removeMacaroon();

private:
    boost::filesystem::path macaroonFilePath() const;

    boost::filesystem::path m_userDataDir;
};

template <typename RetrievePolicyT,
    typename PersistPolicyT = MacaroonPersistPolicyNone>
class MacaroonHandler {
public:
    MacaroonHandler(
        RetrievePolicyT retrievePolicy, PersistPolicyT persistPolicy = {})
        : m_macaroonRetrievePolicy{std::move(retrievePolicy)}
        , m_macaroonPersistPolicy{std::move(persistPolicy)}
        , m_macaroon{m_macaroonRetrievePolicy.retrieveMacaroon()}
        , m_restrictedMacaroon{restrictMacaroon(m_macaroon)}
    {
        m_macaroonPersistPolicy.persistMacaroon(m_macaroon);
    }

    /**
     * @return A time-restricted macaroon.
     */
    std::string restrictedMacaroon() const
    {
        return encode62(m_restrictedMacaroon.serialize());
    }

    /**
     * Refreshes a restricted macaroon saved in this handler.
     */
    std::string refreshRestrictedMacaroon()
    {
        LOG_FCALL();

        m_restrictedMacaroon = restrictMacaroon(m_macaroon);
        return restrictedMacaroon();
    }

    void cleanup() { m_macaroonPersistPolicy.removeMacaroon(); }

protected:
    RetrievePolicyT m_macaroonRetrievePolicy;
    PersistPolicyT m_macaroonPersistPolicy;

    macaroons::Macaroon m_macaroon;
    macaroons::Macaroon m_restrictedMacaroon;
};

using OptionsMacaroonHandler =
    MacaroonHandler<MacaroonRetrievePolicyFromOptions>;
using CLIMacaroonHandler =
    MacaroonHandler<MacaroonRetrievePolicyFromCLI, MacaroonPersistPolicyFile>;

} // namespace auth
} // namespace client
} // namespace one

#endif // ONECLIENT_MACAROON_HANDLER_H
