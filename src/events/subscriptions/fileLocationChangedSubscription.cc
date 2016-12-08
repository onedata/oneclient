/**
 * @file fileLocationChangedSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/events.h"
#include "messages/fuse/fileLocation.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

FileLocationChangedSubscription::FileLocationChangedSubscription(
    std::string fileUuid, std::chrono::milliseconds remoteThreshold,
    EventHandler<FileLocationChanged> handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_remoteThreshold{std::move(remoteThreshold)}
    , m_handler{std::move(handler)}
{
}

StreamKey FileLocationChangedSubscription::streamKey() const
{
    return StreamKey::FILE_LOCATION_CHANGED;
}

StreamPtr FileLocationChangedSubscription::createStream(
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator<FileLocationChanged>>();
    auto emitter = std::make_unique<TimedEmitter<FileLocationChanged>>(
        streamKey(), DEFAULT_TIMED_EMITTER_THRESHOLD, manager, scheduler);
    auto handler = std::make_unique<LocalHandler<FileLocationChanged>>(
        std::move(m_handler));

    return std::make_unique<AsyncStream>(
        std::make_unique<TypedStream<FileLocationChanged>>(
            std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string FileLocationChangedSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileLocationChanged', file UUID: '" << m_fileUuid
           << "', time threshold: " << m_remoteThreshold.count() << "ms";
    return stream.str();
}

ProtoSubscriptionPtr FileLocationChangedSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto msg = subscriptionMsg->mutable_file_location_changed();
    msg->set_file_uuid(m_fileUuid);
    msg->set_time_threshold(m_remoteThreshold.count());

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
