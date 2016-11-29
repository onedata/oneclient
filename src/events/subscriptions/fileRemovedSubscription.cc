/**
 * @file fileRemovedSubscription.cc
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

FileRemovedSubscription::FileRemovedSubscription(
    std::string fileUuid, EventHandler handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_routingKey{"FileRemovedEventStream." + m_fileUuid}
    , m_handler{std::move(handler)}
{
}

const std::string &FileRemovedSubscription::routingKey() const
{
    return m_routingKey;
}

StreamPtr FileRemovedSubscription::createStream(std::int64_t streamId,
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator>();
    auto emitter = std::make_unique<CounterEmitter>(1);
    auto handler = std::make_unique<LocalHandler>(std::move(m_handler));

    return std::make_unique<AsyncStream>(std::make_unique<LocalStream>(
        std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string FileRemovedSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRemoved', file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

ProtoSubscriptionPtr FileRemovedSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto msg = subscriptionMsg->mutable_file_removed_subscription();
    msg->set_file_uuid(m_fileUuid);

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
