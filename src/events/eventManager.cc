/**
* @file eventManager.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"
#include "communication/subscriptionData.h"

#include "events/eventManager.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"
#include "events/types/truncateEvent.h"

#include "messages/readEventSubscription.h"
#include "messages/writeEventSubscription.h"
#include "messages/eventSubscriptionCancellation.h"

#include "messages.pb.h"

namespace one {
namespace client {
namespace events {

EventManager::EventManager(std::shared_ptr<Context> context)
{
    auto communicator = std::make_shared<EventCommunicator>(context);
    m_readEventStream =
        std::make_unique<EventStream<ReadEvent>>(context, communicator);
    m_writeEventStream =
        std::make_unique<EventStream<WriteEvent>>(context, communicator);
    auto predicate = [](const clproto::ServerMessage &msg, const bool) {
        return msg.has_event_subscription();
    };

    auto callback =
        [this](const clproto::ServerMessage &msg) { handleServerMessage(msg); };

    m_unsubscribe =
        context->communicator()->subscribe(communication::SubscriptionData{
            std::move(predicate), std::move(callback)});
}

std::unique_ptr<Event> EventManager::createReadEvent(
    const std::string &fileId, off_t offset, size_t size) const
{
    return std::make_unique<ReadEvent>(m_readEventStream, fileId, offset, size);
}

std::unique_ptr<Event> EventManager::createWriteEvent(
    const std::string &fileId, off_t offset, size_t size, off_t fileSize) const
{
    return std::make_unique<WriteEvent>(
        m_writeEventStream, fileId, offset, size, fileSize);
}

std::unique_ptr<Event> EventManager::createTruncateEvent(
    const std::string &fileId, off_t fileSize) const
{
    return std::make_unique<TruncateEvent>(
        m_writeEventStream, fileId, fileSize);
}

void EventManager::handleServerMessage(const clproto::ServerMessage &msg)
{
    auto subscriptionMsg = msg.event_subscription();
    if (subscriptionMsg.has_read_event_subscription()) {
        ReadEventSubscription subscription{msg};
        auto id = m_readEventStream->addSubscription(subscription);
        m_subscriptionCancellations.emplace(
            id, [ subscription = std::move(subscription), this ] {
                m_readEventStream->removeSubscription(subscription);
            });
    }
    else if (subscriptionMsg.has_write_event_subscription()) {
        WriteEventSubscription subscription{msg};
        auto id = m_writeEventStream->addSubscription(subscription);
        m_subscriptionCancellations.emplace(
            id, [ subscription = std::move(subscription), this ] {
                m_writeEventStream->removeSubscription(subscription);
            });
    }
    else if (subscriptionMsg.has_event_subscription_cancellation()) {
        const EventSubscriptionCancellation cancellation{msg};
        auto searchResult = m_subscriptionCancellations.find(cancellation.id());
        if (searchResult != m_subscriptionCancellations.end()) {
            searchResult->second();
            m_subscriptionCancellations.erase(searchResult);
        }
    }
}

} // namespace events
} // namespace client
} // namespace one
