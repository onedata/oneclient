/**
 * @file fsSubscriptions.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "fsSubscriptions.h"
#include "logging.h"
#include "scheduler.h"
#include "events/eventManager.h"
#include "events/subscriptions/fileAttrSubscription.h"
#include "events/subscriptions/fileLocationSubscription.h"

#include <functional>

namespace one {
namespace client {

FsSubscriptions::FsSubscriptions(
    Scheduler &scheduler, events::EventManager &eventManager)
    : m_scheduler{scheduler}
    , m_eventManager{eventManager}
{
}

void FsSubscriptions::addTemporaryFileAttrSubscription(
    const std::string &fileUuid)
{
    DLOG(INFO) << "Adding temporary subscription for attributes of file: "
               << fileUuid;
    addFileAttrSubscription(fileUuid);
    m_scheduler.schedule(FILE_ATTR_SUBSCRIPTION_DURATION,
        [ this, fileUuid = std::move(fileUuid) ] {
            removeFileAttrSubscription(fileUuid);
        });
}

void FsSubscriptions::addFileLocationSubscription(const std::string &fileUuid)
{
    DLOG(INFO) << "Adding subscription for location of file: " << fileUuid;
    typename decltype(m_fileLocationSubscriptions)::accessor acc;
    if (m_fileLocationSubscriptions.insert(acc, fileUuid))
        acc->second.id = sendFileLocationSubscription(fileUuid);
    ++acc->second.counter;
    addFileAttrSubscription(fileUuid);
}

void FsSubscriptions::removeFileLocationSubscription(
    const std::string &fileUuid)
{
    DLOG(INFO) << "Removing subscription for location of file: " << fileUuid;
    removeFileAttrSubscription(fileUuid);
    typename decltype(m_fileLocationSubscriptions)::accessor acc;
    if (m_fileLocationSubscriptions.find(acc, fileUuid)) {
        --acc->second.counter;
        if (acc->second.counter == 0) {
            sendSubscriptionCancellation(acc->second.id);
            m_fileLocationSubscriptions.erase(acc);
        }
    }
}

void FsSubscriptions::addFileAttrSubscription(const std::string &fileUuid)
{
    DLOG(INFO) << "Adding subscription for attributes of file: " << fileUuid;
    typename decltype(m_fileAttrSubscriptions)::accessor acc;
    if (m_fileAttrSubscriptions.insert(acc, fileUuid))
        acc->second.id = sendFileAttrSubscription(fileUuid);
    ++acc->second.counter;
}

void FsSubscriptions::removeFileAttrSubscription(const std::string &fileUuid)
{
    DLOG(INFO) << "Removing subscription for attributes of file: " << fileUuid;
    typename decltype(m_fileAttrSubscriptions)::accessor acc;
    if (m_fileAttrSubscriptions.find(acc, fileUuid)) {
        --acc->second.counter;
        if (acc->second.counter == 0) {
            sendSubscriptionCancellation(acc->second.id);
            m_fileAttrSubscriptions.erase(acc);
        }
    }
}

std::int64_t FsSubscriptions::sendFileAttrSubscription(
    const std::string &fileUuid)
{
    DLOG(INFO) << "Sending subscription for attributes of file: " << fileUuid;
    events::FileAttrSubscription clientSubscription{fileUuid, 1};
    events::FileAttrSubscription serverSubscription{fileUuid, 1};
    return m_eventManager.subscribe(
        std::move(clientSubscription), std::move(serverSubscription));
}

std::int64_t FsSubscriptions::sendFileLocationSubscription(
    const std::string &fileUuid)
{
    DLOG(INFO) << "Sending subscription for location of file: " << fileUuid;
    events::FileLocationSubscription clientSubscription{fileUuid, 1};
    events::FileLocationSubscription serverSubscription{fileUuid, 1};
    return m_eventManager.subscribe(
        std::move(clientSubscription), std::move(serverSubscription));
    return 1;
}

void FsSubscriptions::sendSubscriptionCancellation(std::int64_t id)
{
    DLOG(INFO) << "Sending subscription cancellation for subscription: " << id;
    m_eventManager.unsubscribe(id);
}

} // namespace client
} // namespace one
