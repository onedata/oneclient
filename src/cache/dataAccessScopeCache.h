/**
 * @file dataAccessScopeCache.h
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "../../s3/onezoneRestClient.h"
#include "options/options.h"
#include "util/cdmi.h"
#include "util/uuid.h"

#include <folly/FBString.h>
#include <folly/futures/Future.h>
#include <folly/futures/SharedPromise.h>

#include <chrono>
#include <map>

namespace one {
namespace client {
namespace cache {

using one::rest::onezone::model::DataAccessScope;
using DataAccessScopePtr = std::shared_ptr<DataAccessScope>;

class DataAccessScopeCache {

public:
    DataAccessScopeCache(std::shared_ptr<options::Options> options,
        std::string accessToken,
        std::unique_ptr<one::rest::onezone::OnezoneClient> onezoneClient = {});

    folly::Future<DataAccessScopePtr> getDataAccessScope(
        bool forceUpdate = false);

    std::optional<folly::fbstring> getProviderIdForSpace(
        const folly::fbstring &spaceId);

    std::optional<folly::fbstring> getSpaceIdByName(
        const folly::fbstring &name);

    std::optional<one::rest::onezone::model::Provider> getProviderForSpace(
        const folly::fbstring &spaceId);

    std::optional<one::rest::onezone::model::Provider> getProvider(
        const folly::fbstring &providerId);

    folly::fbvector<folly::fbstring> readdir(
        const size_t maxSize, const off_t off);

    std::vector<rest::onezone::model::UserSpaceDetails> listSpacesForProvider(
        const std::string &providerId, bool forceUpdate = false);

    std::optional<rest::onezone::model::UserSpaceDetails> getSpaceById(
        const folly::fbstring &spaceId);

    bool isSpaceWhitelisted(const folly::fbstring &spaceId);

    /**
     * Checks if a space with a given name is whitelisted.
     */
    bool isSpaceWhitelisted(
        const rest::onezone::model::UserSpaceDetails &space);

    void disambiguateSpaceNames(DataAccessScope &accessScope);

private:
    void setProviderForSpace(
        const folly::fbstring &spaceId, const folly::fbstring &providerId);

    mutable std::mutex m_cacheMutex;
    std::shared_ptr<options::Options> m_options;
    std::unique_ptr<one::rest::onezone::OnezoneClient> m_onezoneRestClient;

    std::string m_accessToken;

    std::unique_ptr<folly::SharedPromise<DataAccessScopePtr>>
        m_dataAccessScopePromise;

    folly::Synchronized<std::map<folly::fbstring /* spaceId */,
        folly::fbstring /* providerId */>>
        m_selectedProviderForSpace;

    std::unordered_set<folly::fbstring> m_whitelistedSpaceNames;
    std::unordered_set<folly::fbstring> m_whitelistedSpaceIds;
    const bool m_showSpaceIdsNotNames;

    std::atomic<std::chrono::time_point<std::chrono::steady_clock>>
        m_lastUpdate;
    std::atomic_bool m_initiatedUpdate{false};
};

} // namespace cache
} // namespace client
} // namespace one
