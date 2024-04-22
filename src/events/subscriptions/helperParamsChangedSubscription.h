/**
 * @file helperParamsChangedSubscription.h
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_HELPER_PARAMS_CHANGED_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_HELPER_PARAMS_CHANGED_SUBSCRIPTION_H

#include "event.h"
#include "remoteSubscription.h"

#include <chrono>

namespace one {
namespace client {
namespace events {

class HelperParamsChanged;

class HelperParamsChangedSubscription : public RemoteSubscription {
public:
    HelperParamsChangedSubscription(
        std::string storageId, EventHandler<HelperParamsChanged> handler);

    StreamKey streamKey() const override;

    StreamPtr createStream(Manager &manager, SequencerManager &seqManager,
        Scheduler &scheduler) const override;

    std::string toString() const override;

    ProtoSubscriptionPtr serialize() const override;

private:
    std::string m_storageId;
    EventHandler<HelperParamsChanged> m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_HELPER_PARAMS_CHANGED_SUBSCRIPTION_H
