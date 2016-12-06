/**
 * @file fileAttrChangedSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/events.h"
#include "messages/fuse/fileAttr.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

FileAttrChangedSubscription::FileAttrChangedSubscription(std::string fileUuid,
    std::chrono::milliseconds remoteThreshold,
    EventHandler<FileAttrChanged> handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_remoteThreshold{std::move(remoteThreshold)}
    , m_handler{std::move(handler)}
{
}

StreamKey FileAttrChangedSubscription::streamKey() const
{
    return StreamKey::FILE_ATTR_CHANGED;
}

StreamPtr FileAttrChangedSubscription::createStream(
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    using namespace std::literals::chrono_literals;

    auto aggregator = std::make_unique<KeyAggregator<FileAttrChanged>>();
    auto emitter = std::make_unique<TimedEmitter<FileAttrChanged>>(
        streamKey(), 500ms, manager, scheduler);
    auto handler =
        std::make_unique<LocalHandler<FileAttrChanged>>(std::move(m_handler));

    return std::make_unique<AsyncStream>(
        std::make_unique<TypedStream<FileAttrChanged>>(
            std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string FileAttrChangedSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileAttr', file UUID: '" << m_fileUuid
           << "', time threshold: " << m_remoteThreshold.count() << "ms";
    return stream.str();
}

ProtoSubscriptionPtr FileAttrChangedSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto msg = subscriptionMsg->mutable_file_attr_changed();
    msg->set_file_uuid(m_fileUuid);
    msg->set_time_threshold(m_remoteThreshold.count());

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
