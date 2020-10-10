/**
 * @file replicaStatusChangedSubscription.cc
 * @author Bartek Kryza
 * @copyright (C) 2020 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/events.h"
#include "messages/fuse/fileAttr.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

ReplicaStatusChangedSubscription::ReplicaStatusChangedSubscription(
    std::string fileUuid, std::chrono::milliseconds remoteThreshold,
    EventHandler<FileAttrChanged> handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_remoteThreshold{remoteThreshold}
    , m_handler{std::move(handler)}
{
}

StreamKey ReplicaStatusChangedSubscription::streamKey() const
{
    return StreamKey::REPLICA_STATUS_CHANGED;
}

StreamPtr ReplicaStatusChangedSubscription::createStream(Manager &manager,
    SequencerManager & /*seqManager*/, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator<FileAttrChanged>>();
    auto emitter = std::make_unique<TimedEmitter<FileAttrChanged>>(
        streamKey(), DEFAULT_TIMED_EMITTER_THRESHOLD, manager, scheduler);
    auto handler = std::make_unique<LocalHandler<FileAttrChanged>>(m_handler);

    return std::make_unique<AsyncStream>(
        std::make_unique<TypedStream<FileAttrChanged>>(
            std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string ReplicaStatusChangedSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'ReplicaStatusChanged', file UUID: '" << m_fileUuid
           << "', time threshold: " << m_remoteThreshold.count() << "ms";
    return stream.str();
}

ProtoSubscriptionPtr ReplicaStatusChangedSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto msg = subscriptionMsg->mutable_replica_status_changed();
    msg->set_file_uuid(m_fileUuid);
    msg->set_time_threshold(m_remoteThreshold.count());

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
