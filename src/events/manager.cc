/**
 * @file manager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "context.h"
#include "events.h"
#include "logging.h"
#include "messages/configuration.h"
#include "scheduler.h"

namespace one {
namespace client {
namespace events {

Manager::Manager(std::shared_ptr<Context> context)
    : m_scheduler{*context->scheduler()}
    , m_sequencerManager{context->communicator()}
    , m_sequencerStream{m_sequencerManager.create()}
    , m_router{*this, *context->communicator()}
{
}

void Manager::emit(EventPtr<> event)
{
    StreamConstAcc streamAcc;
    if (m_streams.find(streamAcc, event->streamKey())) {
        streamAcc->second->process(std::move(event));
    }
}

std::int64_t Manager::subscribe(const Subscription &subscription)
{
    LOG_FCALL() << LOG_FARG(subscription.toString());

    return subscribe(--m_nextSubscriptionId, subscription);
}

std::int64_t Manager::subscribe(
    std::int64_t subscriptionId, const Subscription &subscription)
{
    LOG_FCALL() << LOG_FARG(subscriptionId)
                << LOG_FARG(subscription.toString());

    StreamAcc streamAcc;
    if (m_streams.insert(streamAcc, subscription.streamKey())) {
        LOG_DBG(2) << "Creating stream '" << subscription.streamKey()
                   << "' for subscription " << subscription.toString();

        streamAcc->second = std::make_unique<SharedStream>(
            subscription.createStream(*this, m_sequencerManager, m_scheduler));
    }
    else {
        streamAcc->second->share();
    }
    streamAcc.release();

    LOG_DBG(2) << "Adding subscription " << subscription.toString()
               << " with ID: '" << subscriptionId << "'";

    HandleAcc handleAcc;
    m_handles.insert(handleAcc, subscriptionId);
    handleAcc->second = subscription.createHandle(
        subscriptionId, m_streams, *m_sequencerStream);

    return subscriptionId;
}

void Manager::subscribe(const messages::Configuration &configuration)
{
    LOG_FCALL() << LOG_FARG(configuration.toString());

    for (const auto &subscription : configuration.subscriptions()) {
        m_router.handle(subscription);
    }
}

bool Manager::unsubscribe(std::int64_t subscriptionId)
{
    LOG_FCALL() << LOG_FARG(subscriptionId);

    HandleAcc handleAcc;
    if (m_handles.find(handleAcc, subscriptionId)) {
        LOG_DBG(2) << "Removing subscription with ID: '" << subscriptionId
                   << "'";

        m_handles.erase(handleAcc);

        return true;
    }
    return false;
}

bool Manager::existsSubscription(std::int64_t subscriptionId)
{
    HandleConstAcc handleAcc;
    return m_handles.find(handleAcc, subscriptionId);
}

void Manager::flush()
{
    LOG_FCALL();

    for (int it = static_cast<int>(StreamKey::FILE_READ);
         it != static_cast<int>(StreamKey::TEST); ++it) {
        flush(static_cast<StreamKey>(it));
    }
}

void Manager::flush(StreamKey streamKey)
{
    LOG_FCALL() << LOG_FARG(streamKey);

    StreamConstAcc streamAcc;
    if (m_streams.find(streamAcc, streamKey)) {
        streamAcc->second->flush();
    }
}

} // namespace events
} // namespace client
} // namespace one
