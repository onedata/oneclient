/**
 * @file fsSubscriptions.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_FS_SUBSCRIPTIONS_H
#define ONECLIENT_FS_SUBSCRIPTIONS_H

#include <folly/FBString.h>

#include <chrono>
#include <cstddef>
#include <string>
#include <unordered_map>

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
     * Constructor.
     * @param eventManager @c EventManager instance.
     */
    FsSubscriptions(events::EventManager &eventManager);

    /**
     * Adds subscription for file location updates. Subscription of the
     * file will be forwarded to the server.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void addFileLocationSubscription(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for file location updates. Subscription cancellation
     * message will be sent to the server.
     * @param fileUuid UUID of file for which subscription is removed.
     * @return True if subscription has been removed, false if it didn't exist
     */
    bool removeFileLocationSubscription(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for permission updates. Subscription of the
     * file will be forwarded to the server.
     * @param fileUuid UUID of file for which subscription is added.
     */
    virtual void addPermissionChangedSubscription(
        const folly::fbstring &fileUuid);

    /**
     * Removes subscription for permission updates. Subscription cancellation
     * message will be sent to the server.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    virtual void removePermissionChangedSubscription(
        const folly::fbstring &fileUuid);

    /**
     * Adds subscription for removing file.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void addFileRemovedSubscription(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for file removal.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    void removeFileRemovedSubscription(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for renaming file.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void addFileRenamedSubscription(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for renaming file.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    void removeFileRenamedSubscription(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for file attributes updates. Subscription of the
     * file will be forwarded to the server.
     * @param fileUuid UUID of file for which subscription is added.
     */
    void addFileAttrSubscription(const folly::fbstring &fileUuid);

    /**
     * Removes subscription for file attributes updates. Subscription
     * cancellation message will be sent to the server.
     * @param fileUuid UUID of file for which subscription is removed.
     */
    void removeFileAttrSubscription(const folly::fbstring &fileUuid);

    /**
     * Adds subscription for quota updates. Subscription of the
     * file will be forwarded to the server.
     */
    void addQuotaSubscription();

    /**
     * Removes subscription for quota updates. Subscription
     * cancellation message will be sent to the server.
     */
    void removeQuotaSubscription();

private:
    std::int64_t sendFileAttrSubscription(const folly::fbstring &fileUuid);
    std::int64_t sendFileLocationSubscription(const folly::fbstring &fileUuid);
    std::int64_t sendPermissionChangedSubscription(
        const folly::fbstring &fileUuid);
    std::int64_t sendFileRemovedSubscription(const folly::fbstring &fileUuid);
    std::int64_t sendQuotaSubscription();
    std::int64_t sendFileRenamedSubscription(const folly::fbstring &fileUuid);
    void sendSubscriptionCancellation(std::int64_t id);

    events::EventManager &m_eventManager;
    std::unordered_map<folly::fbstring, std::int64_t> m_fileAttrSubscriptions;
    std::unordered_map<folly::fbstring, std::int64_t>
        m_fileLocationSubscriptions;
    std::unordered_map<folly::fbstring, std::int64_t>
        m_permissionChangedSubscriptions;
    std::unordered_map<folly::fbstring, std::int64_t>
        m_fileRemovedSubscriptions;
    std::unordered_map<folly::fbstring, std::int64_t>
        m_fileRenamedSubscriptions;

    std::int64_t m_quotaSubscription = 0;
};

} // namespace client
} // namespace one

#endif // ONECLIENT_FS_SUBSCRIPTIONS_H
