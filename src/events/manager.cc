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
#include "messages/fuse/fileAttr.h"
#include "messages/fuse/fileLocation.h"
#include "scheduler.h"

#include "messages.pb.h"

#include <cassert>

namespace one {
namespace client {
namespace events {

Manager::Manager(std::shared_ptr<Context> context)
    : m_scheduler{*context->scheduler()}
    , m_sequencerManager{context->communicator()}
    , m_sequencerStream{m_sequencerManager.create()}
{
    auto predicate = [](const clproto::ServerMessage &msg, const bool) {
        return msg.has_events() || msg.has_subscription() ||
            msg.has_subscription_cancellation();
    };
    auto callback = [this](const clproto::ServerMessage &msg) {
        if (msg.has_events())
            handle(msg.events());
        else if (msg.has_subscription())
            handle(msg.subscription());
        else if (msg.has_subscription_cancellation())
            handle(msg.subscription_cancellation());
    };
    context->communicator()->subscribe(communication::SubscriptionData{
        std::move(predicate), std::move(callback)});
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
    return subscribe(--m_nextSubscriptionId, subscription);
}

std::int64_t Manager::subscribe(
    std::int64_t subscriptionId, const Subscription &subscription)
{
    StreamAcc streamAcc;
    if (m_streams.insert(streamAcc, subscription.streamKey())) {
        DLOG(INFO) << "Creating stream '" << subscription.streamKey()
                   << "' for subscription " << subscription.toString();

        streamAcc->second = std::make_unique<SharedStream>(
            subscription.createStream(*this, m_sequencerManager, m_scheduler));
    }
    else {
        streamAcc->second->share();
    }
    streamAcc.release();

    DLOG(INFO) << "Adding subscription " << subscription.toString()
               << " with ID: '" << subscriptionId << "'";

    HandleAcc handleAcc;
    m_handles.insert(handleAcc, subscriptionId);
    handleAcc->second = subscription.createHandle(
        subscriptionId, m_streams, *m_sequencerStream);

    return subscriptionId;
}

void Manager::subscribe(const messages::Configuration &configuration)
{
    for (const auto &subscription : configuration.subscriptions()) {
        handle(subscription);
    }
}

bool Manager::unsubscribe(std::int64_t subscriptionId)
{
    DLOG(INFO) << "Removing subscription with ID: '" << subscriptionId << "'";

    HandleAcc handleAcc;
    if (m_handles.find(handleAcc, subscriptionId)) {
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

void Manager::flush(StreamKey streamKey)
{
    StreamConstAcc streamAcc;
    if (m_streams.find(streamAcc, streamKey)) {
        streamAcc->second->flush();
    }
}

void Manager::handle(const ProtoEvents &msg)
{
    using namespace messages::fuse;

    for (const auto &eventMsg : msg.events()) {
        if (eventMsg.has_file_attr_changed()) {
            emit(std::make_unique<FileAttrChanged>(
                eventMsg.file_attr_changed()));
        }
        if (eventMsg.has_file_location_changed()) {
            emit(std::make_unique<FileLocationChanged>(
                eventMsg.file_location_changed()));
        }
        else if (eventMsg.has_file_perm_changed()) {
            emit(std::make_unique<FilePermChanged>(
                eventMsg.file_perm_changed()));
        }
        else if (eventMsg.has_file_removed()) {
            emit(std::make_unique<FileRemoved>(eventMsg.file_removed()));
        }
        else if (eventMsg.has_file_renamed()) {
            emit(std::make_unique<FileRenamed>(eventMsg.file_renamed()));
        }
        else if (eventMsg.has_quota_exceeded()) {
            emit(std::make_unique<QuotaExceeded>(eventMsg.quota_exceeded()));
        }
    }
}

void Manager::handle(const ProtoSubscription &msg)
{
    if (msg.has_file_read()) {
        subscribe(msg.id(), FileReadSubscription{msg.file_read()});
    }
    else if (msg.has_file_written()) {
        subscribe(msg.id(), FileWrittenSubscription{msg.file_written()});
    }
}

void Manager::handle(const ProtoCancellation &msg) { unsubscribe(msg.id()); }

} // namespace events
} // namespace client
} // namespace one
