/**
 * @file dataAccessScopeCache.h
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "dataAccessScopeCache.h"

#include "helpers/logging.h"
#include "helpers/storageHelper.h"

namespace one {
namespace client {
namespace cache {

namespace detail {

std::set<std::string> filterAllowedProviderIds(
    const one::rest::onezone::model::DataAccessScope &newAccessScope,
    const std::vector<one::client::options::Endpoint> &allowedProviders)
{
    std::set<std::string> allowedProviderIds;

    for (const auto &allowedProvider : allowedProviders) {
        for (const auto &[providerId, providerDetails] :
            newAccessScope.providers) {
            if (allowedProvider.host == providerDetails.host) {
                allowedProviderIds.emplace(providerId);
                break;
            }
        }
    }

    return allowedProviderIds;
}

boost::optional<std::string> getPreferredProviderId(
    const one::rest::onezone::model::DataAccessScope &newAccessScope,
    const std::vector<one::client::options::Endpoint> &preferredProviders)
{
    boost::optional<std::string> result;

    for (const auto &preferredProvider : preferredProviders) {
        for (const auto &[providerId, providerDetails] :
            newAccessScope.providers) {
            if (preferredProvider.host == providerDetails.host) {
                result = providerId;
                break;
            }
        }
    }

    return result;
}
} // namespace detail

DataAccessScopeCache::DataAccessScopeCache(
    std::shared_ptr<options::Options> options,
    std::unique_ptr<one::rest::onezone::OnezoneClient> onezoneClient)
    : m_options{std::move(options)}
    , m_onezoneRestClient{std::move(onezoneClient)}
    , m_accessToken{m_options->getAccessToken().value()}
    , m_showSpaceIdsNotNames{m_options->showSpaceIds()}
{
    for (const auto &name : m_options->getSpaceNames()) {
        m_whitelistedSpaceNames.emplace(name);
    }
    for (const auto &id : m_options->getSpaceIds()) {
        m_whitelistedSpaceIds.emplace(id);
    }
}

folly::Future<DataAccessScopePtr> DataAccessScopeCache::getDataAccessScope(
    bool forceUpdate)
{
    LOG_FCALL();

    using namespace std::chrono_literals;

    std::lock_guard<std::mutex> lock{m_cacheMutex};

    if (!m_initiatedUpdate.load() &&
        (!m_dataAccessScopePromise || forceUpdate)) {
        m_initiatedUpdate.store(true);
        m_dataAccessScopePromise =
            std::make_unique<folly::SharedPromise<DataAccessScopePtr>>();
    }

    if (!m_dataAccessScopePromise->isFulfilled()) {
        m_dataAccessScopePromise->setWith(
            [this, preferredProviders = m_options->getPreferredProviders(),
                allowedProviders = m_options->getAllowedProviders()]() {
                auto newAccessScope =
                    m_onezoneRestClient->inferAccessTokenScope(m_accessToken);

                m_lastUpdate = std::chrono::steady_clock::now();

                std::set<std::string> allowedProviderIds =
                    detail::filterAllowedProviderIds(
                        newAccessScope, allowedProviders);

                // Find preferred provider Id if one was provided
                boost::optional<std::string> preferredProviderId =
                    detail::getPreferredProviderId(
                        newAccessScope, preferredProviders);

                std::unordered_set<std::string> skippedSpaceIds;

                for (const auto &[spaceId, userSpace] : newAccessScope.spaces) {
                    auto spaceAllowedProviders = userSpace.providers;

                    for (auto it = spaceAllowedProviders.begin();
                         it != spaceAllowedProviders.end();) {
                        if (!allowedProviders.empty() &&
                            allowedProviderIds.find(it->first) ==
                                allowedProviderIds.end()) {
                            it = spaceAllowedProviders.erase(it);
                        }
                        else {
                            ++it;
                        }
                    }

                    if (spaceAllowedProviders.begin() !=
                        spaceAllowedProviders.end()) {
                        std::string selectedProviderId;

                        // Find providerId for spaceId if preferred provider was
                        // set
                        if (preferredProviderId &&
                            spaceAllowedProviders.count(*preferredProviderId) >
                                0) {
                            selectedProviderId = *preferredProviderId;
                        }
                        else {
                            selectedProviderId =
                                spaceAllowedProviders.begin()->first;
                        }

                        if (newAccessScope.providers.count(
                                selectedProviderId) != 0U) {
                            setProviderForSpace(spaceId, selectedProviderId);
                        }
                        else {
                            skippedSpaceIds.emplace(spaceId);
                        }
                    }
                    else {
                        skippedSpaceIds.emplace(spaceId);
                    }
                }

                // Filter skipped spaces
                for (auto it = newAccessScope.spaces.begin();
                     it != newAccessScope.spaces.end();) {
                    if (skippedSpaceIds.find(it->first) !=
                        skippedSpaceIds.end()) {
                        it = newAccessScope.spaces.erase(it);
                    }
                    else {
                        ++it;
                    }
                }

                disambiguateSpaceNames(newAccessScope);

                m_initiatedUpdate.store(false);

                return std::make_shared<DataAccessScope>(
                    std::move(newAccessScope));
            });
    }

    return m_dataAccessScopePromise->getFuture();
}

std::optional<folly::fbstring> DataAccessScopeCache::getProviderIdForSpace(
    const folly::fbstring &spaceId)
{
    LOG_FCALL() << LOG_FARG(spaceId);

    auto selectedProviders = m_selectedProviderForSpace.rlock();

    if (selectedProviders->count(spaceId) == 0)
        return {};

    return selectedProviders->at(spaceId);
}

std::optional<folly::fbstring> DataAccessScopeCache::getSpaceIdByName(
    const folly::fbstring &name)
{
    LOG_FCALL() << LOG_FARG(name);

    auto accessScope = getDataAccessScope().get();

    for (const auto &[id, space] : accessScope->spaces) {
        if (name == space.name) {
            return id;
        }
    }

    return {};
}

std::optional<one::rest::onezone::model::Provider>
DataAccessScopeCache::getProviderForSpace(const folly::fbstring &spaceId)
{
    LOG_FCALL() << LOG_FARG(spaceId);

    std::optional<folly::fbstring> providerId;
    {
        auto selectedProviders = m_selectedProviderForSpace.rlock();

        if (selectedProviders->count(spaceId) == 0) {
            return {};
        }

        providerId = selectedProviders->at(spaceId);
    }

    if (providerId) {
        return getProvider(*providerId);
    }

    return {};
}

std::optional<one::rest::onezone::model::Provider>
DataAccessScopeCache::getProvider(const folly::fbstring &providerId)
{
    return getDataAccessScope()
        .thenValue([providerId](auto &&accessScope)
                       -> std::optional<one::rest::onezone::model::Provider> {
            if (accessScope->providers.count(providerId.toStdString()) > 0) {
                return accessScope->providers.at(providerId.toStdString());
            }
            return {};
        })
        .get();
}

folly::fbvector<folly::fbstring> DataAccessScopeCache::readdir(
    const size_t maxSize, const off_t off)
{
    LOG_FCALL() << LOG_FARG(maxSize) << LOG_FARG(off);

    using namespace std::chrono_literals;

    folly::fbvector<folly::fbstring> result;

    bool forceAccessScopeUpdate =
        std::chrono::steady_clock::now() - m_lastUpdate.load() > 10s;

    auto accessScope = getDataAccessScope(forceAccessScopeUpdate).get();

    folly::fbvector<folly::fbstring> whitelistedSpaces;
    whitelistedSpaces.emplace_back(".");
    whitelistedSpaces.emplace_back("..");

    for (const auto &[spaceId, spaceDetails] : accessScope->spaces) {
        if (isSpaceWhitelisted(spaceDetails)) {
            if (m_showSpaceIdsNotNames)
                whitelistedSpaces.emplace_back(spaceId);
            else
                whitelistedSpaces.emplace_back(spaceDetails.name);
        }
    }

    LOG_DBG(4) << "Got whitelisted spaces list: "
               << fmt::format("[{}]", fmt::join(whitelistedSpaces, ","));

    if (off >= static_cast<off_t>(whitelistedSpaces.size())) {
        return {};
    }

    // Calculate the number of elements that can be copied
    size_t actualCount = std::min(maxSize, whitelistedSpaces.size() - off);

    // Resize the result vector to accommodate the new elements
    result.resize(actualCount);

    // Use std::copy_n to copy the elements
    std::copy_n(whitelistedSpaces.begin() + off, actualCount, result.begin());

    LOG_DBG(4) << "Got effective spaces list: "
               << fmt::format("[{}]", fmt::join(result, ","));

    return result;
}

std::optional<rest::onezone::model::UserSpaceDetails>
DataAccessScopeCache::getSpaceById(const folly::fbstring &spaceId)
{
    LOG_FCALL() << LOG_FARG(spaceId);

    auto accessScope = getDataAccessScope().get();

    for (const auto &[id, userSpace] : accessScope->spaces) {
        if (id == spaceId)
            return userSpace;
    }

    return {};
}

bool DataAccessScopeCache::isSpaceWhitelisted(const folly::fbstring &spaceId)
{
    LOG_FCALL() << LOG_FARG(spaceId);

    auto spaceDetails = getSpaceById(spaceId);
    if (!spaceDetails)
        throw one::helpers::makePosixException(ENOENT);

    return isSpaceWhitelisted(*spaceDetails);
}

/**
 * Checks if a space with a given name is whitelisted.
 */
bool DataAccessScopeCache::isSpaceWhitelisted(
    const rest::onezone::model::UserSpaceDetails &space)
{
    LOG_FCALL() << LOG_FARG(space.name);

    if (m_whitelistedSpaceNames.empty() && m_whitelistedSpaceIds.empty())
        return true;

    bool spaceIsWhitelistedByName = m_whitelistedSpaceNames.find(space.name) !=
        m_whitelistedSpaceNames.end();

    bool spaceIsWhitelistedById = m_whitelistedSpaceIds.find(space.spaceId) !=
        m_whitelistedSpaceIds.end();

    LOG_DBG(2) << "Space " << space.name << "(" << space.spaceId << ") is "
               << spaceIsWhitelistedByName << ":" << spaceIsWhitelistedById;

    return spaceIsWhitelistedByName || spaceIsWhitelistedById;
}

void DataAccessScopeCache::disambiguateSpaceNames(DataAccessScope &accessScope)
{
    LOG_FCALL();

    std::unordered_map<std::string, std::vector<std::string>> nameToSpaceIds;

    // Group spaces by name to find duplicates
    for (const auto &[spaceId, spaceDetails] : accessScope.spaces) {
        nameToSpaceIds[spaceDetails.name].push_back(spaceId);
    }

    // Disambiguate spaces with duplicate names
    for (const auto &[spaceName, spaceIds] : nameToSpaceIds) {
        if (spaceIds.size() > 1) {
            // Multiple spaces have the same name, disambiguate them
            for (const auto &spaceId : spaceIds) {
                auto &spaceDetails = accessScope.spaces.at(spaceId);
                spaceDetails.name = fmt::format("{}@{}", spaceName, spaceId);
            }
        }
    }
}

void DataAccessScopeCache::setProviderForSpace(
    const folly::fbstring &spaceId, const folly::fbstring &providerId)
{
    auto selectedProviders = m_selectedProviderForSpace.wlock();
    // Add new mapping or override existing one
    selectedProviders->emplace(spaceId, providerId);
}
} // namespace cache
} // namespace client
} // namespace one
