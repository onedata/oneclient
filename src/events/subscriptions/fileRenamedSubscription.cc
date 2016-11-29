/**
 * @file fileRenamedSubscription.cc
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

FileRenamedSubscription::FileRenamedSubscription(
    std::string fileUuid, EventHandler handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_routingKey{"FileRenamedEventStream." + m_fileUuid}
    , m_handler{std::move(handler)}
{
}

const std::string &FileRenamedSubscription::routingKey() const
{
    return m_routingKey;
}

StreamPtr FileRenamedSubscription::createStream(std::int64_t streamId,
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator>();
    auto emitter = std::make_unique<CounterEmitter>(1);
    auto handler = std::make_unique<LocalHandler>(std::move(m_handler));

    return std::make_unique<AsyncStream>(std::make_unique<LocalStream>(
        std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string FileRenamedSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'FileRenamed', file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

ProtoSubscriptionPtr FileRenamedSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto msg = subscriptionMsg->mutable_file_renamed_subscription();
    msg->set_file_uuid(m_fileUuid);

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
