/**
* @file readEventSubscription.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"
#include "logging.h"

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "events/eventBuffer.h"
#include "events/eventManager.h"
#include "events/types/readEvent.h"
#include "events/messages/readEventSubscription.h"

namespace one {
namespace client {
namespace events {

ReadEventSubscription::ReadEventSubscription(std::string id)
    : m_id{std::move(id)}
{
}

const std::string &ReadEventSubscription::id() const { return m_id; }

const boost::optional<size_t> &ReadEventSubscription::sizeThreshold() const
{
    return m_sizeThreshold;
}

const boost::optional<size_t> &ReadEventSubscription::counterThreshold() const
{
    return m_counterThreshold;
}

const boost::optional<std::chrono::milliseconds> &
ReadEventSubscription::timeThreshold() const
{
    return m_timeThreshold;
}

void ReadEventSubscription::setSizeThreshold(size_t sizeThreshold)
{
    m_sizeThreshold.reset(sizeThreshold);
}

void ReadEventSubscription::setCounterThreshold(size_t counterThreshold)
{
    m_counterThreshold.reset(counterThreshold);
}

void ReadEventSubscription::setTimeThreshold(
    const std::chrono::milliseconds &timeThreshold)
{
    m_timeThreshold.reset(timeThreshold);
}

bool ReadEventSubscription::process(EventManager &manager) const
{
    manager.subscribe(*this);
    return true;
}

std::unique_ptr<EventMessage>
ReadEventSubscriptionSerializer::deserialize(const Message &message) const
{
    one::clproto::events::ReadEventSubscription readEventSubscription{};
    if (readEventSubscription.ParseFromString(message.worker_answer())) {
        auto eventMessage =
            std::make_unique<one::client::events::ReadEventSubscription>(
                readEventSubscription.id());
        if (readEventSubscription.has_counter_threshold())
            eventMessage->setCounterThreshold(
                readEventSubscription.counter_threshold());
        if (readEventSubscription.has_time_threshold())
            eventMessage->setTimeThreshold(std::chrono::milliseconds{
                readEventSubscription.time_threshold()});
        if (readEventSubscription.has_size_threshold())
            eventMessage->setSizeThreshold(
                readEventSubscription.size_threshold());
        return std::move(eventMessage);
    }
    LOG(WARNING) << "Cannot deserialize message of type: '"
                 << message.message_type()
                 << "' with ID: " << message.message_id();
    return nullptr;
}

std::unique_ptr<EventStream> ReadEventSubscription::createEventStream(
    const EventSubscription &subscription, std::weak_ptr<Context> context,
    std::weak_ptr<EventBuffer> buffer) const
{
    auto readEventSubscription =
        static_cast<const ReadEventSubscription &>(subscription);
    return std::make_unique<ReadEventStream>(
        readEventSubscription, std::move(context), std::move(buffer));
}

} // namespace events
} // namespace client
} // namespace one
