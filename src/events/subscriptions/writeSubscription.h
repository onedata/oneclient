/**
 * @file writeSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H

#include "subscription.h"

#include <boost/optional.hpp>

#include <chrono>

namespace one {
namespace clproto {
class WriteSubscription;
} // namespace clproto
namespace client {
namespace events {

class WriteSubscription : public Subscription {
    using ProtocolMessage = clproto::WriteSubscription;

public:
    WriteSubscription(const ProtocolMessage &msg);

    const std::string &routingKey() const override;

    StreamPtr createStream(std::int64_t streamId, Manager &manager,
        SequencerManager &seqManager, Scheduler &scheduler) const override;

    std::string toString() const override;

private:
    boost::optional<std::size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
    std::string m_routingKey{"WriteEventStream"};
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_WRITE_SUBSCRIPTION_H
