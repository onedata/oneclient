/**
 * @file fileReadSubscription.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_READ_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_READ_SUBSCRIPTION_H

#include "subscription.h"

#include <boost/optional.hpp>

#include <chrono>

namespace one {
namespace clproto {
class FileReadSubscription;
} // namespace clproto
namespace client {
namespace events {

/**
 * @c FileReadSubscription represents a subscription for a read file operation
 * events that occure in the system.
 */
class FileReadSubscription : public Subscription {
    using ProtocolMessage = clproto::FileReadSubscription;

public:
    /**
     * Constructor.
     * @param msg A Protocol Buffers message representing
     * @c FileReadSubscription counterpart.
     */
    FileReadSubscription(const ProtocolMessage &msg);

    StreamKey streamKey() const override;

    /**
     * Creates a stream that aggregates event as long as counter or time
     * thresholds are not exceeded.
     * @see Subscription::createHandle(std::int64_t subscriptionId, Streams
     * &streams, SequencerStream &stream)
     */
    StreamPtr createStream(Manager &manager, SequencerManager &seqManager,
        Scheduler &scheduler) const override;

    std::string toString() const override;

private:
    boost::optional<std::size_t> m_counterThreshold;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_FILE_READ_SUBSCRIPTION_H
