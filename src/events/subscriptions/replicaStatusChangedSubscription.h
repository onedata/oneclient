/**
 * @file replicaStatusChangedSubscription.h
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_SUBSCRIPTIONS_REPLICA_STATUS_CHANGED_SUBSCRIPTION_H
#define ONECLIENT_EVENTS_SUBSCRIPTIONS_REPLICA_STATUS_CHANGED_SUBSCRIPTION_H

#include "event.h"
#include "remoteSubscription.h"

#include <chrono>

namespace one {
namespace client {
namespace events {

/**
 * @c ReplicaStatusChangedSubscription represents a subscription for a change in
 * files replication status (e.g. full replica available).
 */
class ReplicaStatusChangedSubscription : public RemoteSubscription {
public:
    /**
     * Constructor.
     * @param fileUuid An UUID of a file for which attributes change events
     * should be handled.
     * @param handler A callback function that should be executed when file
     * attributes change events occure.
     */
    ReplicaStatusChangedSubscription(std::string fileUuid,
        std::chrono::milliseconds remoteThreshold,
        EventHandler<FileAttrChanged> handler);

    StreamKey streamKey() const override;

    /**
     * Creates a stream that aggregates event as long as time thresholds is not
     * exceeded.
     * @see Subscription::createHandle(std::int64_t subscriptionId, Streams
     * &streams, SequencerStream &stream)
     */
    StreamPtr createStream(Manager &manager, SequencerManager &seqManager,
        Scheduler &scheduler) const override;

    std::string toString() const override;

    ProtoSubscriptionPtr serialize() const override;

private:
    std::string m_fileUuid;
    std::chrono::milliseconds m_remoteThreshold;
    EventHandler<FileAttrChanged> m_handler;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_SUBSCRIPTIONS_REPLICA_STATUS_CHANGED_SUBSCRIPTION_H
