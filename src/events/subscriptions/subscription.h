/**
 * @file subscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H

#include "events/declarations.h"

#include <string>

namespace one {
class Scheduler;
namespace client {
namespace events {

class Manager;

class Subscription {
public:
    virtual ~Subscription() = default;

    virtual const std::string &routingKey() const = 0;

    virtual StreamPtr createStream(std::int64_t streamId, Manager &manager,
        SequencerManager &seqManager, Scheduler &scheduler) const = 0;

    virtual SubscriptionHandlePtr createHandle(std::int64_t subscriptionId,
        std::int64_t streamId, Router &router, SequencerStream &stream) const;

    virtual std::string toString() const = 0;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_SUBSCRIPTION_H
