/**
 * @file s3Subscriptions.cc
 * @author Bartek Kryza
 * @copyright (C) 2025 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "s3Subscriptions.h"

#include "cache/forceProxyIOCache.h"
#include "cache/helpersCache.h"
#include "events/events.h"
#include "scheduler.h"

#include <sstream>

namespace one {
namespace s3 {

S3Subscriptions::S3Subscriptions(one::client::events::Manager &eventManager,
    one::client::cache::HelpersCacheThreadSafeAdapter &helpersCache,
    std::shared_ptr<folly::IOThreadPoolExecutor> executor)
    : m_eventManager{eventManager}
    , m_helpersCache{helpersCache}
    , m_executor{std::move(executor)}
{
}

void S3Subscriptions::unsubscribeAll()
{
    SubscriptionAcc subscriptionAcc;
    m_subscriptions.clear();
}

void S3Subscriptions::subscribeHelperParamsChanged(
    const folly::fbstring &storageId)
{
    LOG_FCALL() << LOG_FARG(storageId);

    ONE_METRIC_COUNTER_INC(
        "comp.ones3.mod.events.submod.subscriptions.helper_params_changed");

    LOG_DBG(2) << "Subscribing for HelperParamsChanged for storage "
               << storageId;

    subscribe(storageId,
        one::client::events::HelperParamsChangedSubscription{
            storageId.toStdString(), [this](auto events) mutable {
                this->handleHelperParamsChangedSubscription(std::move(events));
            }});
}

bool S3Subscriptions::unsubscribeHelperParamsChanged(
    const folly::fbstring &storageId)
{
    LOG_FCALL() << LOG_FARG(storageId);

    ONE_METRIC_COUNTER_DEC(
        "comp.ones3.mod.events.submod.subscriptions.helper_params_changed");

    LOG_DBG(2) << "Unsubscribing for HelperParamsChanged for storage "
               << storageId;

    return unsubscribe(
        one::client::events::StreamKey::HELPER_PARAMS_CHANGED, storageId);
}

void S3Subscriptions::handleHelperParamsChangedSubscription(
    one::client::events::Events<one::client::events::HelperParamsChanged>
        events)
{
    ONE_METRIC_COUNTER_INC(
        "comp.ones3.mod.events.submod.received.helper_params_changed");

    if (m_stopped)
        return;

    if (!events.empty()) {
        m_helpersCache.refreshHelperParameters(events.front()->storageId());
    }
}

void S3Subscriptions::subscribe(const folly::fbstring &fileUuid,
    const one::client::events::Subscription &subscription)
{
    SubscriptionAcc subscriptionAcc;
    if (m_subscriptions.insert(
            subscriptionAcc, {subscription.streamKey(), fileUuid})) {
        // If the subscription doesn't exist yet, create new stream and
        // store its id
        subscriptionAcc->second = m_eventManager.subscribe(subscription);
    }
}

bool S3Subscriptions::isSubscribed(one::client::events::StreamKey streamKey,
    const folly::fbstring &fileUuid) const
{
    SubscriptionAcc subscriptionAcc;
    return m_subscriptions.find(subscriptionAcc, {streamKey, fileUuid});
}

bool S3Subscriptions::unsubscribe(
    one::client::events::StreamKey streamKey, const folly::fbstring &fileUuid)
{
    SubscriptionAcc subscriptionAcc;
    if (m_subscriptions.find(subscriptionAcc, {streamKey, fileUuid})) {
        m_eventManager.unsubscribe(subscriptionAcc->second);
        m_subscriptions.erase(subscriptionAcc);
        return true;
    }
    return false;
}

} // namespace s3
} // namespace one
