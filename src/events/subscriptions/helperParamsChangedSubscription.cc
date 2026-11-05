/**
 * @file helperParamsChangedSubscription.cc
 * @author Bartek Kryza
 * @copyright (C) 2024 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "helperParamsChangedSubscription.h"
#include "events/events.h"
#include "messages/fuse/helperParams.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

HelperParamsChangedSubscription::HelperParamsChangedSubscription(
    std::string storageId, EventHandler<HelperParamsChanged> handler)
    : m_storageId{std::move(storageId)}
    , m_handler{std::move(handler)}
{
}

StreamKey HelperParamsChangedSubscription::streamKey() const
{
    return StreamKey::HELPER_PARAMS_CHANGED;
}

StreamPtr HelperParamsChangedSubscription::createStream(Manager &manager,
    SequencerManager & /*seqManager*/, Scheduler &scheduler) const
{
    auto aggregator = std::make_unique<KeyAggregator<HelperParamsChanged>>();

    auto emitter = std::make_unique<TimedEmitter<HelperParamsChanged>>(
        streamKey(), DEFAULT_TIMED_EMITTER_THRESHOLD, manager, scheduler);
    auto handler =
        std::make_unique<LocalHandler<HelperParamsChanged>>(m_handler);

    return std::make_unique<AsyncStream>(
        std::make_unique<TypedStream<HelperParamsChanged>>(
            std::move(aggregator), std::move(emitter), std::move(handler)));
}

std::string HelperParamsChangedSubscription::toString() const
{
    std::stringstream stream;
    stream << "type: 'HelperParamsChanged', storageId: '" << m_storageId;
    return stream.str();
}

ProtoSubscriptionPtr HelperParamsChangedSubscription::serialize() const
{
    auto subscriptionMsg = std::make_unique<clproto::Subscription>();
    auto *msg = subscriptionMsg->mutable_helper_params_changed();
    msg->set_storage_id(m_storageId);

    return subscriptionMsg;
}

} // namespace events
} // namespace client
} // namespace one
