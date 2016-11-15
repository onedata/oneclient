/**
 * @file fsSubscriptions.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsSubscriptions.h"
#include "events/eventManager.h"
#include "events/subscriptions/fileAttrSubscription.h"
#include "events/subscriptions/fileLocationSubscription.h"
#include "events/subscriptions/permissionChangedSubscription.h"
#include "events/subscriptions/quotaSubscription.h"
#include "scheduler.h"

#include <functional>

namespace one {
namespace client {

FsSubscriptions::FsSubscriptions(events::EventManager &eventManager)
    : m_eventManager{eventManager}
{
}

void FsSubscriptions::addFileLocationSubscription(
    const folly::fbstring &fileUuid)
{
    if (!m_fileLocationSubscriptions.count(fileUuid))
        m_fileLocationSubscriptions[fileUuid] =
            sendFileLocationSubscription(fileUuid);
}

bool FsSubscriptions::removeFileLocationSubscription(
    const folly::fbstring &fileUuid)
{
    auto it = m_fileLocationSubscriptions.find(fileUuid);
    if (it == m_fileLocationSubscriptions.end())
        return false;

    sendSubscriptionCancellation(it->second);
    m_fileLocationSubscriptions.erase(it);
    return true;
}

void FsSubscriptions::addPermissionChangedSubscription(
    const folly::fbstring &fileUuid)
{
    if (!m_permissionChangedSubscriptions.count(fileUuid))
        m_permissionChangedSubscriptions[fileUuid] =
            sendPermissionChangedSubscription(fileUuid);
}

void FsSubscriptions::removePermissionChangedSubscription(
    const folly::fbstring &fileUuid)
{
    auto it = m_permissionChangedSubscriptions.find(fileUuid);
    if (it != m_permissionChangedSubscriptions.end()) {
        sendSubscriptionCancellation(it->second);
        m_permissionChangedSubscriptions.erase(it);
    }
}

void FsSubscriptions::addFileRemovalSubscription(
    const folly::fbstring &fileUuid)
{
    if (!m_fileRemovalSubscriptions.count(fileUuid))
        m_fileRemovalSubscriptions[fileUuid] =
            sendFileRemovalSubscription(fileUuid);
}

void FsSubscriptions::removeFileRemovalSubscription(
    const folly::fbstring &fileUuid)
{
    auto it = m_fileRemovalSubscriptions.find(fileUuid);
    if (it != m_fileRemovalSubscriptions.end()) {
        sendSubscriptionCancellation(it->second);
        m_fileRemovalSubscriptions.erase(it);
    }
}

void FsSubscriptions::addFileRenamedSubscription(
    const folly::fbstring &fileUuid)
{
    if (!m_fileRenamedSubscriptions.count(fileUuid))
        m_fileRenamedSubscriptions[fileUuid] =
            sendFileRenamedSubscription(fileUuid);
}

void FsSubscriptions::removeFileRenamedSubscription(
    const folly::fbstring &fileUuid)
{
    auto it = m_fileRenamedSubscriptions.find(fileUuid);
    if (it != m_fileRenamedSubscriptions.end()) {
        sendSubscriptionCancellation(it->second);
        m_fileRenamedSubscriptions.erase(it);
    }
}

void FsSubscriptions::addFileAttrSubscription(const folly::fbstring &fileUuid)
{
    if (!m_fileAttrSubscriptions.count(fileUuid))
        m_fileAttrSubscriptions[fileUuid] = sendFileAttrSubscription(fileUuid);
}

void FsSubscriptions::removeFileAttrSubscription(
    const folly::fbstring &fileUuid)
{
    auto it = m_fileAttrSubscriptions.find(fileUuid);
    if (it != m_fileAttrSubscriptions.end()) {
        sendSubscriptionCancellation(it->second);
        m_fileAttrSubscriptions.erase(it);
    }
}

void FsSubscriptions::addQuotaSubscription()
{
    m_quotaSubscription = sendQuotaSubscription();
}

void FsSubscriptions::removeQuotaSubscription()
{
    if (m_quotaSubscription) {
        sendSubscriptionCancellation(m_quotaSubscription);
        m_quotaSubscription = 0;
    }
}

std::int64_t FsSubscriptions::sendFileAttrSubscription(
    const folly::fbstring &fileUuid)
{
    DLOG(INFO) << "Sending subscription for change of attributes of file: "
               << fileUuid;
    events::FileAttrSubscription clientSubscription{
        fileUuid.toStdString(), {}, std::chrono::milliseconds{500}};
    events::FileAttrSubscription serverSubscription{
        fileUuid.toStdString(), {}, std::chrono::milliseconds{500}};
    return m_eventManager.subscribe(
        std::move(clientSubscription), std::move(serverSubscription));
}

std::int64_t FsSubscriptions::sendFileLocationSubscription(
    const folly::fbstring &fileUuid)
{
    DLOG(INFO) << "Sending subscription for change of location of file: "
               << fileUuid;
    events::FileLocationSubscription clientSubscription{
        fileUuid.toStdString(), {}, std::chrono::milliseconds{500}};
    events::FileLocationSubscription serverSubscription{
        fileUuid.toStdString(), {}, std::chrono::milliseconds{500}};
    return m_eventManager.subscribe(
        std::move(clientSubscription), std::move(serverSubscription));
}

std::int64_t FsSubscriptions::sendPermissionChangedSubscription(
    const folly::fbstring &fileUuid)
{
    DLOG(INFO) << "Sending subscription for change of permissions of file: "
               << fileUuid;
    events::PermissionChangedSubscription clientSubscription{
        fileUuid.toStdString()};
    events::PermissionChangedSubscription serverSubscription{
        fileUuid.toStdString()};
    return m_eventManager.subscribe(
        std::move(clientSubscription), std::move(serverSubscription));
}

std::int64_t FsSubscriptions::sendFileRemovalSubscription(
    const folly::fbstring &fileUuid)
{
    DLOG(INFO) << "Sending subscription for removing file: " << fileUuid;
    events::FileRemovalSubscription clientSubscription{fileUuid.toStdString()};
    events::FileRemovalSubscription serverSubscription{fileUuid.toStdString()};
    return m_eventManager.subscribe(
        std::move(clientSubscription), std::move(serverSubscription));
}

std::int64_t FsSubscriptions::sendQuotaSubscription()
{
    DLOG(INFO) << "Sending subscription for quota";
    events::QuotaSubscription clientSubscription{};
    events::QuotaSubscription serverSubscription{};
    return m_eventManager.subscribe(
        std::move(clientSubscription), std::move(serverSubscription));
}

std::int64_t FsSubscriptions::sendFileRenamedSubscription(
    const folly::fbstring &fileUuid)
{
    DLOG(INFO) << "Sending subscription for renaming file: " << fileUuid;
    events::FileRenamedSubscription clientSubscription{fileUuid.toStdString()};
    events::FileRenamedSubscription serverSubscription{fileUuid.toStdString()};
    return m_eventManager.subscribe(
        std::move(clientSubscription), std::move(serverSubscription));
}

void FsSubscriptions::sendSubscriptionCancellation(std::int64_t id)
{
    m_eventManager.unsubscribe(id);
}

} // namespace client
} // namespace one
