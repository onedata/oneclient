/**
 * @file eventManager.cc
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#include "events/eventManager.h"

#include "context.h"
#include "communication/subscriptionData.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"
#include "logging.h"
#include "messages.pb.h"
#include "messages/readEventSubscription.h"
#include "messages/writeEventSubscription.h"
#include "messages/eventSubscriptionCancellation.h"

namespace one {
namespace client {
namespace events {

EventManager::EventManager(std::shared_ptr<Context> context)
    : m_context{std::move(context)}
{
    auto communicator = std::make_shared<EventCommunicator>(m_context);
    m_readEventStream =
        std::make_unique<EventStream<ReadEvent>>(m_context, communicator);
    m_writeEventStream =
        std::make_unique<EventStream<WriteEvent>>(m_context, communicator);

    auto predicate = [](const clproto::ServerMessage &msg, const bool) {
        return msg.has_event_subscription();
    };
    auto callback =
        [this](const clproto::ServerMessage &msg) { handleServerMessage(msg); };

    m_context->communicator()->subscribe(communication::SubscriptionData{
        std::move(predicate), std::move(callback)});
}

void EventManager::emitReadEvent(
    off_t offset, size_t size, std::string fileUuid) const
{
    m_context->scheduler()
        ->post([ =, fileUuid = std::move(fileUuid) ]() mutable {
            ReadEvent event{offset, size, std::move(fileUuid)};
            m_readEventStream->pushAsync(std::move(event));
        });
}

void EventManager::emitWriteEvent(off_t offset, std::size_t size,
    std::string fileUuid, std::string storageId, std::string fileId) const
{
    m_context->scheduler()->post([
        =,
        fileUuid = std::move(fileUuid),
        storageId = std::move(storageId),
        fileId = std::move(fileId)
    ]() mutable {
        WriteEvent event{offset, size, std::move(fileUuid), 1,
            std::move(storageId), std::move(fileId)};

        m_writeEventStream->pushAsync(std::move(event));
    });
}

void EventManager::emitTruncateEvent(off_t fileSize, std::string fileUuid) const
{
    m_context->scheduler()
        ->post([ =, fileUuid = std::move(fileUuid) ]() mutable {
            TruncateEvent event{fileSize, std::move(fileUuid)};
            m_writeEventStream->pushAsync(std::move(event));
        });
}

void EventManager::handleServerMessage(const clproto::ServerMessage &msg)
{
    const auto &subscriptionMsg = msg.event_subscription();
    if (subscriptionMsg.has_read_event_subscription()) {
        ReadEventSubscription subscription{msg};
        auto id = m_readEventStream->addSubscriptionAsync(subscription);
        m_subscriptionsCancellation.emplace(
            id, [ this, subscription = std::move(subscription) ] {
                m_readEventStream->removeSubscriptionAsync(
                    std::move(subscription));
            });
    }
    else if (subscriptionMsg.has_write_event_subscription()) {
        WriteEventSubscription subscription{msg};
        auto id = m_writeEventStream->addSubscriptionAsync(subscription);
        m_subscriptionsCancellation.emplace(
            id, [ this, subscription = std::move(subscription) ] {
                m_writeEventStream->removeSubscriptionAsync(
                    std::move(subscription));
            });
    }
    else if (subscriptionMsg.has_event_subscription_cancellation()) {
        EventSubscriptionCancellation cancellation{msg};
        auto searchResult = m_subscriptionsCancellation.find(cancellation.id());
        if (searchResult != m_subscriptionsCancellation.end()) {
            searchResult->second();
            m_subscriptionsCancellation.erase(searchResult);
        }
    }
}

} // namespace events
} // namespace client
} // namespace one
