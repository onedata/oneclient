/**
* @file eventManager.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"
#include "logging.h"

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "events/types/event.h"
#include "events/eventMapper.h"
#include "events/eventBuffer.h"
#include "events/eventFactory.h"
#include "events/eventManager.h"
#include "events/eventCommunicator.h"

#include "events/messages/eventMessage.h"
#include "events/messages/eventEmissionRequest.h"
#include "events/messages/readEventSubscription.h"
#include "events/messages/writeEventSubscription.h"
#include "events/messages/subscriptionCancellation.h"
#include "events/messages/eventEmissionConfirmation.h"

#include <boost/algorithm/string/predicate.hpp>

namespace one {
namespace client {
namespace events {

EventManager::EventManager(std::shared_ptr<Context> context)
    : m_context{std::move(context)}
    , m_factory{std::make_shared<EventFactory>()}
    , m_communicator{std::make_shared<EventCommunicator>(std::move(context))}
    , m_buffer{std::make_shared<EventBuffer>(m_communicator)}
    , m_mapper{std::make_shared<EventMapper>(std::move(context))}
{
}

const EventFactory &EventManager::eventFactory() const { return *m_factory; }

void EventManager::emit(unsigned long long id)
{
    try {
        const auto &message = m_buffer->getSentMessage(id);
        m_communicator->send(message);
    }
    catch (const std::exception &e) {
        LOG(WARNING) << "Cannot emit message with ID: " << id
                     << " due to: " << e.what();
    }
}

void EventManager::emit(const Event &event) { m_mapper->map(event); }

void EventManager::removeConfirmedEvents(unsigned long long id)
{
    m_buffer->removeSentMessages(id);
}

bool EventManager::handle(const Message &message)
{
    const auto &messageType = message.message_type();
    std::unique_ptr<EventMessageSerializer> serializer;

    if (boost::iequals(messageType, READ_EVENT_SUBSCRIPTION_MESSAGE)) {
        serializer = std::make_unique<ReadEventSubscriptionSerializer>();
    } else if (boost::iequals(messageType, WRITE_EVENT_SUBSCRIPTION_MESSAGE)) {
        serializer = std::make_unique<WriteEventSubscriptionSerializer>();
    } else if (boost::iequals(messageType, SUBSCRIPTION_CANCELLATION_MESSAGE)) {
        serializer = std::make_unique<SubscriptionCancellationSerializer>();
    } else if (boost::iequals(messageType, EVENT_EMISSION_REQUEST_MESSAGE)) {
        serializer = std::make_unique<EventEmissionRequestSerializer>();
    } else if (boost::iequals(messageType,
                              EVENT_EMISSION_CONFIRMATION_MESSAGE)) {
        serializer = std::make_unique<EventEmissionConfirmationSerializer>();
    }

    auto eventMessage = serializer->deserialize(message);
    if (eventMessage)
        return eventMessage->process(*this);
    return true;
}

const std::string &
EventManager::subscribe(const EventSubscription &subscription)
{
    auto stream =
        subscription.createEventStream(subscription, m_context, m_buffer);
    return m_mapper->addOrUpdateEventStream(*stream);
}

bool EventManager::unsubscribe(const std::string &id)
{
    return m_mapper->removeEventStream(id);
}

} // namespace events
} // namespace client
} // namespace one
