/**
 * @file fsSubscriptions.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FS_SUBSCRIPTIONS_H
#define ONECLIENT_FS_SUBSCRIPTIONS_H

#include <tbb/concurrent_hash_map.h>

#include <chrono>
#include <cstddef>
#include <string>

namespace one {
class Scheduler;
namespace client {
namespace events {
class EventManager;
} // namespace events

constexpr std::chrono::seconds FILE_ATTR_SUBSCRIPTION_DURATION{30};

class FsSubscriptions {
public:
    /**
     * The @c SubscriptionHandle class stores subscription ID along with a
     * subscription reference counter.
     */
    struct SubscriptionHandle {
        std::int64_t id;
        std::uint32_t counter;
    };

    /**
     * Constructor.
     * @param scheduler @c Scheduler instance.
     * @param eventManager @c EventManager instance.
     */
    FsSubscriptions(Scheduler &scheduler, events::EventManager &eventManager);

    /**
     * Adds temporary subscription for file attributes updates. If it is the
     * first subscription of the file it will be forwarded to the server.
     * Subscription will be removed after @c FILE_ATTR_SUBSCRIPTION_DURATION
     * seconds and if it was the last subscription for attributes of the file
     * a subscription cancellation message will be sent to the server.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void addTemporaryFileAttrSubscription(const std::string &fileUuid);

    /**
     * Adds subscription for file location updates. If it is the first
     * subscription of the file it will be forwarded to the server.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void addFileLocationSubscription(const std::string &fileUuid);

    /**
     * Removes subscription for file location updates.  If it is the last
     * subscription of the file a subscription cancellation message will be sent
     * to the server.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    void removeFileLocationSubscription(const std::string &fileUuid);

    /**
     * Adds subscription for permission updates. If it is the first
     * subscription of the file it will be forwarded to the server.
     * @param fileUuid UUID of file for which subscription is added.
     */
    virtual void addPermissionChangedSubscription(const std::string &fileUuid);

    /**
     * Removes subscription for permission updates.  If it is the last
     * subscription of the file a subscription cancellation message will be sent
     * to the server.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    virtual void removePermissionChangedSubscription(
        const std::string &fileUuid);

private:
    void addFileAttrSubscription(const std::string &fileUuid);
    void removeFileAttrSubscription(const std::string &fileUuid);
    std::int64_t sendFileAttrSubscription(const std::string &fileUuid);
    std::int64_t sendFileLocationSubscription(const std::string &fileUuid);
    std::int64_t sendPermissionChangedSubscription(const std::string &fileUuid);
    void sendSubscriptionCancellation(std::int64_t id);

    Scheduler &m_scheduler;
    events::EventManager &m_eventManager;
    tbb::concurrent_hash_map<std::string, SubscriptionHandle>
        m_fileAttrSubscriptions;
    tbb::concurrent_hash_map<std::string, SubscriptionHandle>
        m_fileLocationSubscriptions;
    tbb::concurrent_hash_map<std::string, SubscriptionHandle>
        m_permissionChangedSubscriptions;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_FS_SUBSCRIPTIONS_H
