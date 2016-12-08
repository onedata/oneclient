/**
 * @file filePermChangedSubscription.cc
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

FilePermChangedSubscription::FilePermChangedSubscription(
    std::string fileUuid, EventHandler<FilePermChanged> handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_handler{std::move(handler)}
{
}

StreamKey FilePermChangedSubscription::streamKey() const
{
    return StreamKey::FILE_PERM_CHANGED;
}

StreamPtr FilePermChangedSubscription::createStream(
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator<FilePermChanged>>();
    auto emitter = std::make_unique<CounterEmitter<FilePermChanged>>(1);
    auto handler =
        std::make_unique<LocalHandler<FilePermChanged>>(std::move(m_handler));

    return std::make_unique<AsyncStream>(
        std::make_unique<TypedStream<FilePermChanged>>(
            std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string FilePermChangedSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'FilePermChanged', file UUID: '" << m_fileUuid << "'";
    return stream.str();
}

ProtoSubscriptionPtr FilePermChangedSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto msg = subscriptionMsg->mutable_file_perm_changed();
    msg->set_file_uuid(m_fileUuid);

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
