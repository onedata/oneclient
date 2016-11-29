/**
 * @file fsSubscriptions.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FS_SUBSCRIPTIONS_H
#define ONECLIENT_FS_SUBSCRIPTIONS_H

#include "events/declarations.h"

#include <folly/FBString.h>

#include <tbb/concurrent_hash_map.h>

namespace one {
namespace client {
namespace cache {
class ForceProxyIOCache;
class LRUMetadataCache;
} // namespace cache
namespace events {
class Manager;
} // namespace events

constexpr auto FILE_ATTR_PREFIX = "FileAttr";
constexpr auto FILE_LOCATION_PREFIX = "FileLocation";
constexpr auto PERMISSION_CHANGED_PREFIX = "FileLocation";
constexpr auto FILE_REMOVED_PREFIX = "FileLocation";
constexpr auto FILE_RENAMED_PREFIX = "FileLocation";
constexpr auto QUOTA_EXCEEDED_PREFIX = "QuotaExceeded";
constexpr std::chrono::seconds SUBSCRIPTION_DURATION{30};

class FsSubscriptions {
public:
    /**
     * Constructor.
     * @param eventManager @c events::Manager instance.
     * @param metadataCache @c cache::LRUMetadataCache instance.
     * @param forceProxyIOCache @c cache::ForceProxyIOCache instance.
     * @param runInFiber A function that runs callback inside a main fiber.
     */
    FsSubscriptions(events::Manager &eventManager,
        cache::LRUMetadataCache &metadataCache,
        cache::ForceProxyIOCache &forceProxyIOCache,
        std::function<void(folly::Function<void()>)> runInFiber);

    /**
     * Adds subscription for file attributes updates.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFileAttrChanged(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for file attributes updates.
     * @param fileUuid UUID of file for which subscription is removed.
     * @return true if subscription has been removed, false if it didn't exist.
     */
    bool unsubscribeFileAttrChanged(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for file location updates.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFileLocationChanged(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for file location updates.
     * @param fileUuid UUID of file for which subscription is removed.
     * @return true if subscription has been removed, false if it didn't exist.
     */
    bool unsubscribeFileLocationChanged(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for file permission changed event.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribePermissionChanged(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for file permission changed event.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    bool unsubscribePermissionChanged(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for file removed event.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFileRemoved(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for file removed event.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    bool unsubscribeFileRemoved(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for file renamed event.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFileRenamed(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for file renamed event.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    bool unsubscribeFileRenamed(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for quota updates.
     */
    void subscribeQuotaExceeded(events::EventHandler handler);

private:
    std::string getKey(
        const folly::fbstring &prefix, const folly::fbstring &fileUuid);
    void subscribe(
        const std::string &key, events::ConstSubscriptionPtr subscription);
    bool unsubscribe(const std::string &key);

    void handleFileAttrChanged(std::vector<events::EventPtr> events);
    void handleFileLocationChanged(std::vector<events::EventPtr> events);
    void handlePermissionChanged(std::vector<events::EventPtr> events);
    void handleFileRemoved(std::vector<events::EventPtr> events);
    void handleFileRenamed(std::vector<events::EventPtr> events);

    events::Manager &m_eventManager;
    cache::LRUMetadataCache &m_metadataCache;
    cache::ForceProxyIOCache &m_forceProxyIOCache;
    std::function<void(folly::Function<void()>)> m_runInFiber;
    tbb::concurrent_hash_map<std::string, std::int64_t> m_streams;
    tbb::concurrent_hash_map<std::string, std::int64_t> m_subscriptions;

    using StreamAcc = typename decltype(m_streams)::accessor;
    using SubscriptionAcc = typename decltype(m_subscriptions)::accessor;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_FS_SUBSCRIPTIONS_H
