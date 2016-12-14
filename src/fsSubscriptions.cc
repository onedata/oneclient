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
    subscribe(
        fileUuid, events::FileAttrChangedSubscription{fileUuid.toStdString(),
                      SUBSCRIPTION_DURATION, [this](auto events) mutable {
                          this->handleFileAttrChanged(std::move(events));
                      }});
}

void FsSubscriptions::handleFileAttrChanged(
    events::Events<events::FileAttrChanged> events)
{
    using namespace messages::fuse;

    m_runInFiber([ this, events = std::move(events) ] {
        for (auto &event : events) {
            auto &attr = event->fileAttr();
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
    return unsubscribe(events::StreamKey::FILE_ATTR_CHANGED, fileUuid);
}

void FsSubscriptions::subscribeFileLocationChanged(
    const folly::fbstring &fileUuid)
{
    subscribe(fileUuid,
        events::FileLocationChangedSubscription{fileUuid.toStdString(),
            SUBSCRIPTION_DURATION, [this](auto events) mutable {
                this->handleFileLocationChanged(std::move(events));
            }});
}

void FsSubscriptions::handleFileLocationChanged(
    events::Events<events::FileLocationChanged> events)
{
    using namespace messages::fuse;

    m_runInFiber([ this, events = std::move(events) ] {
        for (auto &event : events) {
            auto &loc = event->fileLocation();
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
    return unsubscribe(events::StreamKey::FILE_LOCATION_CHANGED, fileUuid);
}

void FsSubscriptions::subscribeFilePermChanged(const folly::fbstring &fileUuid)
{
    subscribe(
        fileUuid, events::FilePermChangedSubscription{
                      fileUuid.toStdString(), [this](auto events) mutable {
                          this->handlePermissionChanged(std::move(events));
                      }});
}

void FsSubscriptions::handlePermissionChanged(
    events::Events<events::FilePermChanged> events)
{
    m_runInFiber([ this, events = std::move(events) ] {
        for (auto &event : events) {
            m_forceProxyIOCache.remove(event->fileUuid());
        }
    });
}

bool FsSubscriptions::unsubscribeFilePermChanged(
    const folly::fbstring &fileUuid)
{
    return unsubscribe(events::StreamKey::FILE_PERM_CHANGED, fileUuid);
}

void FsSubscriptions::subscribeFileRemoved(const folly::fbstring &fileUuid)
{
    subscribe(fileUuid, events::FileRemovedSubscription{fileUuid.toStdString(),
                            [this](auto events) mutable {
                                this->handleFileRemoved(std::move(events));
                            }});
}

void FsSubscriptions::handleFileRemoved(
    events::Events<events::FileRemoved> events)
{
    m_runInFiber([ this, events = std::move(events) ] {
        for (auto &event : events) {
            auto &uuid = event->fileUuid();
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
    return unsubscribe(events::StreamKey::FILE_REMOVED, fileUuid);
}

void FsSubscriptions::subscribeFileRenamed(const folly::fbstring &fileUuid)
{
    subscribe(fileUuid, events::FileRenamedSubscription{fileUuid.toStdString(),
                            [this](auto events) mutable {
                                this->handleFileRenamed(std::move(events));
                            }});
}

void FsSubscriptions::handleFileRenamed(
    events::Events<events::FileRenamed> events)
{
    m_runInFiber([ this, events = std::move(events) ] {
        for (auto &event : events) {
            auto &entry = event->topEntry();
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
    return unsubscribe(events::StreamKey::FILE_RENAMED, fileUuid);
}

void FsSubscriptions::subscribe(
    const folly::fbstring &fileUuid, const events::Subscription &subscription)
{
    SubscriptionAcc subscriptionAcc;
    if (m_subscriptions.insert(
            subscriptionAcc, {subscription.streamKey(), fileUuid})) {
        subscriptionAcc->second = m_eventManager.subscribe(subscription);
    }
}

bool FsSubscriptions::unsubscribe(
    events::StreamKey streamKey, const folly::fbstring &fileUuid)
{
    SubscriptionAcc subscriptionAcc;
    if (m_subscriptions.find(subscriptionAcc, {streamKey, fileUuid})) {
        m_eventManager.unsubscribe(subscriptionAcc->second);
        m_subscriptions.erase(subscriptionAcc);
        return true;
    }
    return false;
}

} // namespace client
} // namespace one
