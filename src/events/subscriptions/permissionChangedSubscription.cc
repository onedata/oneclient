/**
 * @file permissionChangedSubscription.cc
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

PermissionChangedSubscription::PermissionChangedSubscription(
    std::string fileUuid, EventHandler handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_routingKey{"PermissionChangedEventStream." + m_fileUuid}
    , m_handler{std::move(handler)}
{
}

const std::string &PermissionChangedSubscription::routingKey() const
{
    return m_routingKey;
}

StreamPtr PermissionChangedSubscription::createStream(std::int64_t streamId,
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator>();
    auto emitter = std::make_unique<CounterEmitter>(1);
    auto handler = std::make_unique<LocalHandler>(std::move(m_handler));

    return std::make_unique<AsyncStream>(std::make_unique<LocalStream>(
        std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string PermissionChangedSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'PermissionChanged', file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

ProtoSubscriptionPtr PermissionChangedSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto msg = subscriptionMsg->mutable_permission_changed_subscription();
    msg->set_file_uuid(m_fileUuid);

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
