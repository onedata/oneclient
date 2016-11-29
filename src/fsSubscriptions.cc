/**
 * @file fsSubscriptions.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsSubscriptions.h"
#include "cache/forceProxyIOCache.h"
#include "cache/lruMetadataCache.h"
#include "events/events.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"
#include "scheduler.h"

#include <cassert>
#include <sstream>

namespace one {
namespace client {

FsSubscriptions::FsSubscriptions(events::Manager &eventManager,
    cache::LRUMetadataCache &metadataCache,
    cache::ForceProxyIOCache &forceProxyIOCache,
    std::function<void(folly::Function<void()>)> runInFiber)
    : m_eventManager{eventManager}
    , m_metadataCache{metadataCache}
    , m_forceProxyIOCache{forceProxyIOCache}
    , m_runInFiber{std::move(runInFiber)}
{
}

void FsSubscriptions::subscribeFileAttrChanged(const folly::fbstring &fileUuid)
{
    subscribe(getKey(FILE_ATTR_PREFIX, fileUuid),
        std::make_shared<const events::FileAttrSubscription>(
            fileUuid.toStdString(), SUBSCRIPTION_DURATION,
            [this](auto events) mutable {
                this->handleFileAttrChanged(std::move(events));
            }));
}

void FsSubscriptions::handleFileAttrChanged(
    std::vector<events::EventPtr> events)
{
    using namespace messages::fuse;

    m_runInFiber([ this, events = std::move(events) ] {
        for (auto &event : events) {
            auto e = events::get<events::UpdateEvent<FileAttr>>(event);
            auto &attr = e->wrapped();
            if (m_metadataCache.updateAttr(attr))
                LOG(INFO) << "Updated attributes for uuid: '" << attr.uuid()
                          << "', size: " << (attr.size() ? *attr.size() : -1);
            else
                LOG(INFO) << "No attributes to update for uuid: '"
                          << attr.uuid() << "'";
        }
    });
}

bool FsSubscriptions::unsubscribeFileAttrChanged(
    const folly::fbstring &fileUuid)
{
    return unsubscribe(getKey(FILE_ATTR_PREFIX, fileUuid));
}

void FsSubscriptions::subscribeFileLocationChanged(
    const folly::fbstring &fileUuid)
{
    subscribe(getKey(FILE_LOCATION_PREFIX, fileUuid),
        std::make_shared<const events::FileLocationSubscription>(
            fileUuid.toStdString(), SUBSCRIPTION_DURATION,
            [this](auto events) mutable {
                this->handleFileLocationChanged(std::move(events));
            }));
}

void FsSubscriptions::handleFileLocationChanged(
    std::vector<events::EventPtr> events)
{
    using namespace messages::fuse;

    m_runInFiber([ this, events = std::move(events) ] {
        for (auto &event : events) {
            auto e = events::get<events::UpdateEvent<FileLocation>>(event);
            auto &loc = e->wrapped();
            if (m_metadataCache.updateLocation(loc))
                LOG(INFO) << "Updated locations for uuid: '" << loc.uuid()
                          << "'";
            else
                LOG(INFO) << "No location to update for uuid: '" << loc.uuid()
                          << "'";
        }
    });
}

bool FsSubscriptions::unsubscribeFileLocationChanged(
    const folly::fbstring &fileUuid)
{
    return unsubscribe(getKey(FILE_LOCATION_PREFIX, fileUuid));
}

void FsSubscriptions::subscribePermissionChanged(
    const folly::fbstring &fileUuid)
{
    subscribe(getKey(PERMISSION_CHANGED_PREFIX, fileUuid),
        std::make_shared<const events::PermissionChangedSubscription>(
            fileUuid.toStdString(), [this](auto events) mutable {
                this->handlePermissionChanged(std::move(events));
            }));
}

void FsSubscriptions::handlePermissionChanged(
    std::vector<events::EventPtr> events)
{
    m_runInFiber([ this, events = std::move(events) ] {
        for (auto event : events) {
            auto e = events::get<events::PermissionChangedEvent>(event);
            m_forceProxyIOCache.remove(e->fileUuid());
        }
    });
}

bool FsSubscriptions::unsubscribePermissionChanged(
    const folly::fbstring &fileUuid)
{
    return unsubscribe(getKey(PERMISSION_CHANGED_PREFIX, fileUuid));
}

void FsSubscriptions::subscribeFileRemoved(const folly::fbstring &fileUuid)
{
    subscribe(getKey(FILE_REMOVED_PREFIX, fileUuid),
        std::make_shared<const events::FileRemovedSubscription>(
            fileUuid.toStdString(), [this](auto events) mutable {
                this->handleFileRemoved(std::move(events));
            }));
}

void FsSubscriptions::handleFileRemoved(std::vector<events::EventPtr> events)
{
    m_runInFiber([ this, events = std::move(events) ] {
        for (auto event : events) {
            auto e = events::get<events::FileRemovedEvent>(event);
            auto &uuid = e->fileUuid();
            if (m_metadataCache.markDeleted(uuid))
                LOG(INFO) << "File remove event received: " << uuid;
            else
                LOG(INFO) << "Received a file remove event for '" << uuid
                          << "', but the file metadata is no longer cached.";
        }
    });
}

bool FsSubscriptions::unsubscribeFileRemoved(const folly::fbstring &fileUuid)
{
    return unsubscribe(getKey(FILE_REMOVED_PREFIX, fileUuid));
}

void FsSubscriptions::subscribeFileRenamed(const folly::fbstring &fileUuid)
{
    subscribe(getKey(FILE_RENAMED_PREFIX, fileUuid),
        std::make_shared<const events::FileRemovedSubscription>(
            fileUuid.toStdString(), [this](auto events) mutable {
                this->handleFileRenamed(std::move(events));
            }));
}

void FsSubscriptions::handleFileRenamed(std::vector<events::EventPtr> events)
{
    m_runInFiber([ this, events = std::move(events) ] {
        for (auto event : events) {
            auto e = events::get<events::FileRenamedEvent>(event);
            auto &entry = e->topEntry();
            if (m_metadataCache.rename(entry.oldUuid(), entry.newParentUuid(),
                    entry.newName(), entry.newUuid()))
                LOG(INFO) << "File renamed event handled: '" << entry.oldUuid()
                          << "' -> '" << entry.newUuid() << "'";
            else
                LOG(INFO) << "Received a file renamed event for '"
                          << entry.oldUuid()
                          << "', but the file metadata is no longer cached.";
        }
    });
}

bool FsSubscriptions::unsubscribeFileRenamed(const folly::fbstring &fileUuid)
{
    return unsubscribe(getKey(FILE_RENAMED_PREFIX, fileUuid));
}

void FsSubscriptions::subscribeQuotaExceeded(events::EventHandler handler)
{
    subscribe(QUOTA_EXCEEDED_PREFIX,
        std::make_shared<const events::QuotaSubscription>(std::move(handler)));
}

std::string FsSubscriptions::getKey(
    const folly::fbstring &prefix, const folly::fbstring &fileUuid)
{
    std::stringstream ss;
    ss << prefix << "." << fileUuid;
    return ss.str();
}

void FsSubscriptions::subscribe(
    const std::string &key, events::ConstSubscriptionPtr subscription)
{
    SubscriptionAcc subscriptionAcc;
    if (m_subscriptions.insert(subscriptionAcc, key)) {
        StreamAcc streamAcc;
        if (m_streams.insert(streamAcc, subscription->routingKey())) {
            streamAcc->second = m_eventManager.createStream(subscription);
        }
        subscriptionAcc->second =
            m_eventManager.subscribe(streamAcc->second, subscription);
    }
}

bool FsSubscriptions::unsubscribe(const std::string &key)
{
    SubscriptionAcc subscriptionAcc;
    if (m_subscriptions.find(subscriptionAcc, key)) {
        m_eventManager.unsubscribe(subscriptionAcc->second);
        m_subscriptions.erase(subscriptionAcc);
        return true;
    }
    return false;
}

} // namespace client
} // namespace one
