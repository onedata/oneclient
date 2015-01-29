/**
* @file eventManager.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "events/types/event.h"
#include "events/types/readEvent.h"
#include "events/types/writeEvent.h"

#include "events/eventBuffer.h"
#include "events/eventFactory.h"
#include "events/eventManager.h"
#include "events/eventCommunicator.h"

#include "events/messages/eventRequest.h"
#include "events/messages/subscriptionCancellation.h"
#include "events/messages/eventAcknowledgement.h"

#include <boost/algorithm/string/predicate.hpp>

namespace one {
namespace client {
namespace events {

EventManager::EventManager(std::shared_ptr<Context> context)
    : m_context{std::move(context)}
    , m_factory{std::make_shared<EventFactory>()}
    , m_communicator{std::make_shared<EventCommunicator>(m_context)}
    , m_buffer{std::make_shared<EventBuffer>(m_communicator)}
    , m_readEventStream{std::make_shared<ReadEventStream>(m_context, m_buffer)}
    , m_writeEventStream{
          std::make_shared<WriteEventStream>(m_context, m_buffer)}
{
}

bool EventManager::handle(const Message &message)
{
    const auto &messageType = message.message_type();

    if (boost::iequals(messageType, READ_EVENT_SUBSCRIPTION_MESSAGE)) {
        ReadEventSubscriptionSerializer serializer{};
        auto eventMessage = serializer.deserialize(message);
        if (eventMessage)
            eventMessage->process(m_readEventStream, m_factory, m_buffer);
    } else if (boost::iequals(messageType, WRITE_EVENT_SUBSCRIPTION_MESSAGE)) {
        WriteEventSubscriptionSerializer serializer{};
        auto eventMessage = serializer.deserialize(message);
        if (eventMessage)
            eventMessage->process(m_writeEventStream, m_factory, m_buffer);
    } else if (boost::iequals(messageType, SUBSCRIPTION_CANCELLATION_MESSAGE)) {
        SubscriptionCancellationSerializer serializer{};
        auto eventMessage = serializer.deserialize(message);
        if (eventMessage)
            eventMessage->process(*this, m_factory, m_buffer);
    } else if (boost::iequals(messageType, EVENT_REQUEST_MESSAGE)) {
        EventRequestSerializer serializer{};
        auto eventMessage = serializer.deserialize(message);
        if (eventMessage)
            eventMessage->process(m_buffer, m_communicator);
    } else if (boost::iequals(messageType, EVENT_ACKNOWLEDGEMENT_MESSAGE)) {
        EventAcknowledgementSerializer serializer{};
        auto eventMessage = serializer.deserialize(message);
        if (eventMessage)
            eventMessage->process(m_buffer);
    }
    return true;
}

bool EventManager::cancelSubscription(unsigned long long id) const
{
    return m_readEventStream->cancelSubscription(id) ||
           m_writeEventStream->cancelSubscription(id);
}

std::unique_ptr<Event> EventManager::createReadEvent(const std::string &fileId,
                                                     off_t offset,
                                                     size_t size) const
{
    return m_factory->createReadEvent(fileId, offset, size, m_readEventStream);
}

std::unique_ptr<Event> EventManager::createWriteEvent(const std::string &fileId,
                                                      off_t offset, size_t size,
                                                      off_t fileSize) const
{
    return m_factory->createWriteEvent(fileId, offset, size, fileSize,
                                       m_writeEventStream);
}

std::unique_ptr<Event>
EventManager::createTruncateEvent(const std::string &fileId,
                                  off_t fileSize) const
{
    return m_factory->createTruncateEvent(fileId, fileSize, m_writeEventStream);
}

} // namespace events
} // namespace client
} // namespace one
