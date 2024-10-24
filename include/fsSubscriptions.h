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
#include "events/streams.h"

#include <folly/FBString.h>

#include <tbb/concurrent_hash_map.h>

namespace one {
namespace client {
namespace cache {
class ForceProxyIOCache;
class OpenFileMetadataCache;
template <typename T> class HelpersCache;
} // namespace cache
namespace events {
class Manager;
class FileAttrChanged;
class FileLocationChanged;
class FilePermChanged;
class FileRemoved;
class FileRenamed;
class HelperParamsChanged;
} // namespace events

constexpr std::chrono::milliseconds REMOTE_TIME_THRESHOLD{500};

template <typename T> struct StdHashCompare {
    bool equal(const T &a, const T &b) const { return a == b; }
    std::size_t hash(const T &a) const
    {
        return std::hash<std::pair<std::size_t, std::size_t>>()(
            {static_cast<std::size_t>(a.first),
                std::hash<folly::fbstring>()(a.second)});
    }
};

class FsSubscriptions {
    using Key = std::pair<events::StreamKey, folly::fbstring>;

public:
    /**
     * Constructor.
     * @param eventManager @c events::Manager instance.
     * @param metadataCache @c cache::OpenFileMetadataCache instance.
     * @param forceProxyIOCache @c cache::ForceProxyIOCache instance.
     * @param runInFiber A function that runs callback inside a main fiber.
     */
    FsSubscriptions(events::Manager &eventManager,
        cache::OpenFileMetadataCache &metadataCache,
        cache::ForceProxyIOCache &forceProxyIOCache,
        cache::HelpersCache<communication::Communicator> &helpersCache,
        std::function<void(folly::Function<void()>)> runInFiber);

    /**
     * Cancel all subscriptions.
     */
    void unsubscribeAll();

    /**
     * Adds subscription for storage parameter update.
     * @param storageId Storage id.
     */
    void subscribeHelperParamsChanged(const folly::fbstring &storageId);

    /**
     * Cancels subscription for storage parameter update.
     * @param storageId Storage id.
     * @return true if subscription has been removed, false if it didn't exist.
     */
    bool unsubscribeHelperParamsChanged(const folly::fbstring &storageId);

    /**
     * Adds subscription for file attributes updates.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFileAttrChanged(const folly::fbstring &fileUuid);

    /**
     * Cancels subscription for file attributes updates.
     * @param fileUuid UUID of file for which subscription is removed.
     * @return true if subscription has been removed, false if it didn't exist.
     */
    bool unsubscribeFileAttrChanged(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for replication status change.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeReplicaStatusChanged(const folly::fbstring &fileUuid);

    /**
     * Cancels subscription for replication status changes.
     * @param fileUuid UUID of file for which subscription is removed.
     * @return true if subscription has been removed, false if it didn't exist.
     */
    bool unsubscribeReplicaStatusChanged(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for file location updates.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFileLocationChanged(const folly::fbstring &fileUuid);

    /**
     * Cancels subscription for file location updates.
     * @param fileUuid UUID of file for which subscription is removed.
     * @return true if subscription has been removed, false if it didn't exist.
     */
    bool unsubscribeFileLocationChanged(const folly::fbstring &fileUuid);

    /**
     * Checks whether a file location subscription is active for a given file.
     * @param fileUuid Uuid of the file to check.
     * @returns True, if file location subscription is active for the given
     *          file.
     */
    bool isSubscribedToFileLocationChanged(
        const folly::fbstring &fileUuid) const;

    /**
     * Adds subscription for file permission changed event.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFilePermChanged(const folly::fbstring &fileUuid);

    /**
     * Cancels subscription for file permission changed event.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    bool unsubscribeFilePermChanged(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for file removed event.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFileRemoved(const folly::fbstring &fileUuid);

    /**
     * Cancels subscription for file removed event.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    bool unsubscribeFileRemoved(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for file renamed event.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void subscribeFileRenamed(const folly::fbstring &fileUuid);

    /**
     * Cancels subscription for file renamed event.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    bool unsubscribeFileRenamed(const folly::fbstring &fileUuid);

    /**
     * Stop handling of events.
     */
    void stop() { m_stopped = true; }

private:
    void subscribe(const folly::fbstring &fileUuid,
        const events::Subscription &subscription);
    bool isSubscribed(
        events::StreamKey streamKey, const folly::fbstring &fileUuid) const;
    bool unsubscribe(
        events::StreamKey streamKey, const folly::fbstring &fileUuid);

    void handleFileAttrChanged(events::Events<events::FileAttrChanged> events);
    void handleFileLocationChanged(
        events::Events<events::FileLocationChanged> events);
    void handlePermissionChanged(
        events::Events<events::FilePermChanged> events);
    void handleFileRemoved(events::Events<events::FileRemoved> events);
    void handleFileRenamed(events::Events<events::FileRenamed> events);

    void handleHelperParamsChangedSubscription(
        events::Events<events::HelperParamsChanged> events);

    events::Manager &m_eventManager;
    cache::OpenFileMetadataCache &m_metadataCache;
    cache::ForceProxyIOCache &m_forceProxyIOCache;
    cache::HelpersCache<communication::Communicator> &m_helpersCache;
    std::function<void(folly::Function<void()>)> m_runInFiber;
    // Holds a mapping of specific subscription keys (composed of stream type
    // and file uuid) to integer subscription id's generated by stream manager
    tbb::concurrent_hash_map<Key, std::int64_t, StdHashCompare<Key>>
        m_subscriptions;

    using SubscriptionAcc = typename decltype(m_subscriptions)::accessor;

    bool m_stopped{false};
};

} // namespace client
} // namespace one

#endif // ONECLIENT_FS_SUBSCRIPTIONS_H
