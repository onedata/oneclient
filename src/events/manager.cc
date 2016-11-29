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

void Manager::emit(ConstEventPtr event)
{
    RouterAcc routerAcc;
    if (m_router.find(routerAcc, event->routingKey())) {
        for (auto streamId : routerAcc->second) {
            StreamConstAcc streamAcc;
            if (m_streams.find(streamAcc, streamId)) {
                streamAcc->second->process(event);
            }
        }
    }
}

std::int64_t Manager::createStream(ConstSubscriptionPtr subscription)
{
    auto streamId = nextStreamId();

    DLOG(INFO) << "Creating stream '" << streamId << "' for subscription "
               << subscription->toString();

    StreamAcc streamAcc;
    m_streams.insert(streamAcc, streamId);
    streamAcc->second = subscription->createStream(
        streamId, *this, m_sequencerManager, m_scheduler);

    return streamId;
}

bool Manager::removeStream(std::int64_t streamId)
{
    DLOG(INFO) << "Removing stream '" << streamId << "'";

    StreamAcc streamAcc;
    if (m_streams.find(streamAcc, streamId)) {
        m_streams.erase(streamAcc);
        return true;
    }
    return false;
}

std::int64_t Manager::subscribe(
    std::int64_t streamId, ConstSubscriptionPtr subscription)
{
    return subscribe(streamId, nextSubscriptionId(), std::move(subscription));
}

std::int64_t Manager::subscribe(std::int64_t streamId,
    std::int64_t subscriptionId, ConstSubscriptionPtr subscription)
{
    DLOG(INFO) << "Adding subscription " << subscription->toString()
               << " with ID: '" << subscriptionId << "'";

    HandleAcc handleAcc;
    m_handles.insert(handleAcc, subscriptionId);
    handleAcc->second = subscription->createHandle(
        subscriptionId, streamId, m_router, *m_sequencerStream);

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

void Manager::flush(std::int64_t streamId)
{
    StreamConstAcc streamAcc;
    if (m_streams.find(streamAcc, streamId)) {
        streamAcc->second->flush();
    }
}

void Manager::handle(const ProtoEvents &msg)
{
    using namespace messages::fuse;

    for (const auto &eventMsg : msg.events()) {
        if (eventMsg.has_update_event()) {
            const auto &updateMsg = eventMsg.update_event();
            if (updateMsg.has_file_attr()) {
                emit(std::make_shared<const UpdateEvent<FileAttr>>(
                    updateMsg.file_attr()));
            }
            else if (updateMsg.has_file_location()) {
                emit(std::make_shared<const UpdateEvent<FileLocation>>(
                    updateMsg.file_location()));
            }
        }
        else if (eventMsg.has_permission_changed_event()) {
            emit(std::make_shared<const PermissionChangedEvent>(
                eventMsg.permission_changed_event()));
        }
        else if (eventMsg.has_file_removed_event()) {
            emit(std::make_shared<const FileRemovedEvent>(
                eventMsg.file_removed_event()));
        }
        else if (eventMsg.has_file_renamed_event()) {
            emit(std::make_shared<const FileRenamedEvent>(
                eventMsg.file_renamed_event()));
        }
        else if (eventMsg.has_quota_exceeded_event()) {
            emit(std::make_shared<const QuotaExceededEvent>(
                eventMsg.quota_exceeded_event()));
        }
    }
}

void Manager::handle(const ProtoSubscription &msg)
{
    ConstSubscriptionPtr subscription;

    if (msg.has_read_subscription()) {
        subscription =
            std::make_shared<const ReadSubscription>(msg.read_subscription());
    }
    else if (msg.has_write_subscription()) {
        subscription =
            std::make_shared<const WriteSubscription>(msg.write_subscription());
    }

    assert(subscription);
    auto streamId = createStream(subscription);
    subscribe(streamId, msg.id(), std::move(subscription));
}

void Manager::handle(const ProtoCancellation &msg) { unsubscribe(msg.id()); }

std::int64_t Manager::nextStreamId() { return m_nextStreamId++; }

std::int64_t Manager::nextSubscriptionId() { return -m_nextSubscriptionId++; }

} // namespace events
} // namespace client
} // namespace one
