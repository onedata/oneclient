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
    std::string fileUuid, EventHandler<FileRemoved> handler)
    : m_fileUuid{std::move(fileUuid)}
    , m_handler{std::move(handler)}
{
}

StreamKey FileRemovedSubscription::streamKey() const
{
    return StreamKey::FILE_REMOVED;
}

StreamPtr FileRemovedSubscription::createStream(
    Manager &manager, SequencerManager &seqManager, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator<FileRemoved>>();
    auto emitter = std::make_unique<CounterEmitter<FileRemoved>>(1);
    auto handler =
        std::make_unique<LocalHandler<FileRemoved>>(std::move(m_handler));

    return std::make_unique<AsyncStream>(
        std::make_unique<TypedStream<FileRemoved>>(
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
    auto msg = subscriptionMsg->mutable_file_removed();
    msg->set_file_uuid(m_fileUuid);

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
