/**
* @file writeEventSubscription.cc
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
#include "events/eventFactory.h"
#include "events/types/writeEvent.h"

namespace one {
namespace client {
namespace events {

WriteEventSubscription::WriteEventSubscription(unsigned long long id)
    : m_id{id}
{
}

unsigned long long WriteEventSubscription::id() const { return m_id; }

const boost::optional<size_t> &WriteEventSubscription::sizeThreshold() const
{
    return m_sizeThreshold;
}

const boost::optional<size_t> &WriteEventSubscription::counterThreshold() const
{
    return m_counterThreshold;
}

const boost::optional<std::chrono::milliseconds> &
WriteEventSubscription::timeThreshold() const
{
    return m_timeThreshold;
}

void WriteEventSubscription::setSizeThreshold(size_t sizeThreshold)
{
    m_sizeThreshold.reset(sizeThreshold);
}

void WriteEventSubscription::setCounterThreshold(size_t counterThreshold)
{
    m_counterThreshold.reset(counterThreshold);
}

void WriteEventSubscription::setTimeThreshold(
    const std::chrono::milliseconds &timeThreshold)
{
    m_timeThreshold.reset(timeThreshold);
}

void WriteEventSubscription::process(std::weak_ptr<WriteEventStream> stream,
                                     std::weak_ptr<EventFactory> factory,
                                     std::weak_ptr<EventBuffer> buffer) const
{
    unsigned long long id = stream.lock()->subscribe(*this);
    auto event = factory.lock()->createSubscriptionEvent(id);
    buffer.lock()->push(std::move(event));
}

std::unique_ptr<WriteEventSubscription>
WriteEventSubscriptionSerializer::deserialize(const Message &message) const
{
    one::clproto::events::WriteEventSubscription writeEventSubscription{};
    if (writeEventSubscription.ParseFromString(message.worker_answer())) {
        auto eventMessage =
            std::make_unique<one::client::events::WriteEventSubscription>(
                writeEventSubscription.id());
        if (writeEventSubscription.has_counter_threshold())
            eventMessage->setCounterThreshold(
                writeEventSubscription.counter_threshold());
        if (writeEventSubscription.has_time_threshold())
            eventMessage->setTimeThreshold(std::chrono::milliseconds{
                writeEventSubscription.time_threshold()});
        if (writeEventSubscription.has_size_threshold())
            eventMessage->setSizeThreshold(
                writeEventSubscription.size_threshold());
        return std::move(eventMessage);
    }
    LOG(WARNING) << "Cannot deserialize message of type: '"
                 << message.message_type()
                 << "' with ID: " << message.message_id();
    return nullptr;
}

} // namespace events
} // namespace client
} // namespace one
