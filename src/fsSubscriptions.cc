/**
 * @file fsSubscriptions.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsSubscriptions.h"
#include "cache/forceProxyIOCache.h"
#include "cache/openFileMetadataCache.h"
#include "events/events.h"
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"
#include "monitoring/monitoring.h"
#include "scheduler.h"

#include <cassert>
#include <sstream>

namespace one {
namespace client {

FsSubscriptions::FsSubscriptions(events::Manager &eventManager,
    cache::OpenFileMetadataCache &metadataCache,
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
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.subscriptions.file_attr_changed");

    LOG_DBG(2) << "Subscribing for FileAttrChanged for file " << fileUuid;

    subscribe(fileUuid,
        events::FileAttrChangedSubscription{fileUuid.toStdString(),
            REMOTE_TIME_THRESHOLD, [this](auto events) mutable {
                this->handleFileAttrChanged(std::move(events));
            }});
}

void FsSubscriptions::handleFileAttrChanged(
    events::Events<events::FileAttrChanged> events)
{
    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.received.file_attr_changed");

    m_runInFiber(
        [this, events = std::move(events)] {
            for (auto &event : events) {
                auto attr = std::make_shared<FileAttr>(event->fileAttr());

                LOG_DBG(2) << " Received FileAttrChanged event: "
                           << attr->toString();

                if (m_metadataCache.updateAttr(attr)) {
                    LOG_DBG(2)
                        << "Updated attributes for uuid: '" << attr->uuid()
                        << "', size: " << (attr->size() ? *attr->size() : -1);
                }
                else {
                    LOG_DBG(2)
                        << "Update or insert of attribute failed for uuid : '"
                        << attr->uuid() << "'";
                }
            }
        });
}

bool FsSubscriptions::unsubscribeFileAttrChanged(
    const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_DEC(
        "comp.oneclient.mod.events.submod.subscriptions.file_attr_changed");
    LOG_DBG(2) << "Unsubscribing for FileAttrChanged for file " << fileUuid;
    return unsubscribe(events::StreamKey::FILE_ATTR_CHANGED, fileUuid);
}

void FsSubscriptions::subscribeFileLocationChanged(
    const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.subscriptions.file_location_changed");
    LOG_DBG(2) << "Subscribing for FileLocationChanged for file " << fileUuid;
    subscribe(fileUuid,
        events::FileLocationChangedSubscription{fileUuid.toStdString(),
            REMOTE_TIME_THRESHOLD, [this](auto events) mutable {
                this->handleFileLocationChanged(std::move(events));
            }});
}

void FsSubscriptions::handleFileLocationChanged(
    events::Events<events::FileLocationChanged> events)
{
    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.received.file_location_changed");
    m_runInFiber([this, events = std::move(events)] {
        for (auto &event : events) {
            bool updateSucceeded = false;

            if (event->changeStartOffset() && event->changeEndOffset())
                updateSucceeded = m_metadataCache.updateLocation(
                    *(event->changeStartOffset()), *(event->changeEndOffset()),
                    event->fileLocation());
            else
                updateSucceeded =
                    m_metadataCache.updateLocation(event->fileLocation());

            if (updateSucceeded)
                LOG_DBG(2) << "Updated locations for uuid: '"
                           << event->fileLocation().uuid() << "'";
            else
                LOG_DBG(2) << "No location to update for uuid: '"
                           << event->fileLocation().uuid() << "'";
        }
    });
}

bool FsSubscriptions::unsubscribeFileLocationChanged(
    const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_DEC(
        "comp.oneclient.mod.events.submod.subscriptions.file_location_changed");
    LOG_DBG(2) << "Unsubscribing for FileLocationChanged for file " << fileUuid;
    return unsubscribe(events::StreamKey::FILE_LOCATION_CHANGED, fileUuid);
}

bool FsSubscriptions::isSubscribedToFileLocationChanged(
    const folly::fbstring &fileUuid) const
{
    return isSubscribed(events::StreamKey::FILE_LOCATION_CHANGED, fileUuid);
}

void FsSubscriptions::subscribeFilePermChanged(const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.subscriptions.file_perm_changed");
    LOG_DBG(2) << "Subscribing for FilePermChanged for file " << fileUuid;
    subscribe(fileUuid,
        events::FilePermChangedSubscription{
            fileUuid.toStdString(), [this](auto events) mutable {
                this->handlePermissionChanged(std::move(events));
            }});
}

void FsSubscriptions::handlePermissionChanged(
    events::Events<events::FilePermChanged> events)
{
    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.received.file_permission_changed");
    m_runInFiber([this, events = std::move(events)] {
        for (auto &event : events) {
            m_forceProxyIOCache.remove(event->fileUuid());
        }
    });
}

bool FsSubscriptions::unsubscribeFilePermChanged(
    const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_DEC(
        "comp.oneclient.mod.events.submod.subscriptions.file_perm_changed");
    LOG_DBG(2) << "Unsubscribing for FilePermChanged for file " << fileUuid;
    return unsubscribe(events::StreamKey::FILE_PERM_CHANGED, fileUuid);
}

void FsSubscriptions::subscribeFileRemoved(const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.subscriptions.file_removed");
    LOG_DBG(2) << "Subscribing for FileRemoved for file " << fileUuid;
    subscribe(fileUuid,
        events::FileRemovedSubscription{
            fileUuid.toStdString(), [this](auto events) mutable {
                this->handleFileRemoved(std::move(events));
            }});
}

void FsSubscriptions::handleFileRemoved(
    events::Events<events::FileRemoved> events)
{
    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.received.file_removed");
    m_runInFiber([this, events = std::move(events)] {
        for (auto &event : events) {
            auto &uuid = event->fileUuid();
            if (m_metadataCache.markDeleted(uuid))
                LOG_DBG(2) << "File remove event received: " << uuid;
            else
                LOG_DBG(2) << "Received a file remove event for '" << uuid
                           << "', but the file metadata is no longer cached.";
        }
    });
}

bool FsSubscriptions::unsubscribeFileRemoved(const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_DEC(
        "comp.oneclient.mod.events.submod.subscriptions.file_removed");
    LOG_DBG(2) << "Unsubscribing for FileRemoved for file " << fileUuid;
    return unsubscribe(events::StreamKey::FILE_REMOVED, fileUuid);
}

void FsSubscriptions::subscribeFileRenamed(const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.subscriptions.file_renamed");

    LOG_DBG(2) << "Subscribing for FileRenamed for file " << fileUuid;

    subscribe(fileUuid,
        events::FileRenamedSubscription{
            fileUuid.toStdString(), [this](auto events) mutable {
                this->handleFileRenamed(std::move(events));
            }});
}

void FsSubscriptions::handleFileRenamed(
    events::Events<events::FileRenamed> events)
{
    LOG_FCALL() << LOG_FARG(events.size());

    ONE_METRIC_COUNTER_INC(
        "comp.oneclient.mod.events.submod.received.file_renamed");
    m_runInFiber([this, events = std::move(events)] {
        for (auto &event : events) {
            auto &entry = event->topEntry();
            if (m_metadataCache.rename(entry.oldUuid(), entry.newParentUuid(),
                    entry.newName(), entry.newUuid()))
                LOG_DBG(2) << "File renamed event handled: '" << entry.oldUuid()
                           << "' -> '" << entry.newUuid() << "'";
            else
                LOG_DBG(2) << "Received a file renamed event for '"
                           << entry.oldUuid()
                           << "', but the file metadata is no longer cached.";
        }
    });
}

bool FsSubscriptions::unsubscribeFileRenamed(const folly::fbstring &fileUuid)
{
    LOG_FCALL() << LOG_FARG(fileUuid);

    ONE_METRIC_COUNTER_DEC(
        "comp.oneclient.mod.events.submod.subscriptions.file_renamed");
    LOG_DBG(2) << "Unsubscribing for FileRenamed for file " << fileUuid;
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

bool FsSubscriptions::isSubscribed(
    events::StreamKey streamKey, const folly::fbstring &fileUuid) const
{
    SubscriptionAcc subscriptionAcc;
    return m_subscriptions.find(subscriptionAcc, {streamKey, fileUuid});
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
