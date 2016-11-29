/**
 * @file fileAttrSubscription.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/events.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

FileAttrSubscription::FileAttrSubscription(std::string fileUuid,
    std::chrono::milliseconds remoteThreshold, EventHandler handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_remoteThreshold{std::move(remoteThreshold)}
    , m_routingKey{"FileAttrEventStream." + m_fileUuid}
    , m_handler{std::move(handler)}
{
}

const std::string &FileAttrSubscription::routingKey() const
{
    return m_routingKey;
}

StreamPtr FileAttrSubscription::createStream(std::int64_t streamId,
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    using namespace std::literals::chrono_literals;

    auto aggregator = std::make_unique<KeyAggregator>();
    auto emitter =
        std::make_unique<TimedEmitter>(streamId, 500ms, manager, scheduler);
    auto handler = std::make_unique<LocalHandler>(std::move(m_handler));

    return std::make_unique<AsyncStream>(std::make_unique<LocalStream>(
        std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string FileAttrSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileAttr', file UUID: '" << m_fileUuid
           << "', time threshold: " << m_remoteThreshold.count() << "ms";
    return stream.str();
}

ProtoSubscriptionPtr FileAttrSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto msg = subscriptionMsg->mutable_file_attr_subscription();
    msg->set_file_uuid(m_fileUuid);
    msg->set_time_threshold(m_remoteThreshold.count());

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
