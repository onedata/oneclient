/**
* @file writeEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"

#include "events.pb.h"

#include "events/eventBuffer.h"
#include "events/types/writeEvent.h"

namespace one {
namespace client {
namespace events {

WriteEvent::WriteEvent(std::string fileId, off_t offset, size_t size,
                       off_t fileSize, std::weak_ptr<WriteEventStream> stream)
    : m_fileId{std::move(fileId)}
    , m_size{size}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(offset,
                                                                offset + size)}
    , m_fileSize{fileSize}
    , m_stream{std::move(stream)}
{
}

WriteEvent &WriteEvent::operator+=(const WriteEvent &event)
{
    m_counter += event.m_counter;
    m_size += event.m_size;
    m_blocks += event.m_blocks;
    m_fileSize += event.m_fileSize;
    return *this;
}

void WriteEvent::emit() { m_stream.lock()->push(*this); }

std::unique_ptr<EventSerializer> WriteEvent::serializer() const
{
    return std::make_unique<WriteEventSerializer>();
}

std::unique_ptr<google::protobuf::Message>
WriteEventSerializer::serialize(unsigned long long id, const Event &event) const
{
    auto writeEvent = static_cast<const WriteEvent &>(event);
    auto message = std::make_unique<one::clproto::events::WriteEvent>();
    message->set_seq_num(id);
    message->set_counter(writeEvent.m_counter);
    message->set_file_id(std::move(writeEvent.m_fileId));
    message->set_file_size(writeEvent.m_fileSize);
    message->set_size(writeEvent.m_size);
    for (const auto &block : writeEvent.m_blocks) {
        auto blockMessage = message->add_blocks();
        blockMessage->set_offset(block.lower());
        blockMessage->set_size(block.upper() - block.lower());
    }
    return std::move(message);
}

WriteEventStream::WriteEventStream(std::weak_ptr<Context> context,
                                   std::weak_ptr<EventBuffer> buffer)
    : m_counter{0}
    , m_counterThreshold{}
    , m_time{}
    , m_timeThreshold{}
    , m_size{0}
    , m_sizeThreshold{}
    , m_context{std::move(context)}
    , m_buffer{std::move(buffer)}
    , m_events{}
    , m_subscriptions{}
    , m_counterThresholds{}
    , m_timeThresholds{}
    , m_sizeThresholds{}
{
}

void WriteEventStream::push(const WriteEvent &event)
{
    if (!m_subscriptions.empty()) {
        m_counter += event.m_counter;
        m_size += event.m_size;

        auto writeEvent = m_events.find(event.m_fileId);
        if (writeEvent != m_events.end())
            writeEvent->second += event;
        else
            m_events.insert(std::make_pair(event.m_fileId, event));

        if (isEmissionRuleSatisfied())
            emit();
    }
}

unsigned long long
WriteEventStream::subscribe(const WriteEventSubscription &subscription)
{
    if (m_subscriptions.find(subscription.m_id) != m_subscriptions.end())
        return subscription.m_id;

    m_subscriptions.insert(std::make_pair(subscription.m_id, subscription));

    bool isTimeThresholdUpdated = false;
    if (subscription.m_counterThreshold) {
        m_counterThresholds.insert(subscription.m_counterThreshold.get());
        if (!m_counterThreshold ||
            m_counterThreshold.get() > subscription.m_counterThreshold.get())
            m_counterThreshold.reset(subscription.m_counterThreshold.get());
    }
    if (subscription.m_timeThreshold) {
        m_timeThresholds.insert(subscription.m_timeThreshold.get());
        if (!m_timeThreshold ||
            m_timeThreshold.get() > subscription.m_timeThreshold.get()) {
            m_timeThreshold.reset(subscription.m_timeThreshold.get());
            isTimeThresholdUpdated = true;
        }
    }
    if (subscription.m_sizeThreshold) {
        m_sizeThresholds.insert(subscription.m_sizeThreshold.get());
        if (!m_sizeThreshold ||
            m_sizeThreshold.get() > subscription.m_sizeThreshold.get())
            m_sizeThreshold.reset(subscription.m_sizeThreshold.get());
    }
    if (isTimeThresholdUpdated || isEmissionRuleSatisfied())
        emit();

    return subscription.m_id;
}

bool WriteEventStream::cancelSubscription(unsigned long long id)
{
    auto subscription = m_subscriptions.find(id);
    if (subscription != m_subscriptions.end()) {
        if (subscription->second.m_counterThreshold)
            m_counterThresholds.erase(
                subscription->second.m_counterThreshold.get());
        if (subscription->second.m_timeThreshold)
            m_timeThresholds.erase(subscription->second.m_timeThreshold.get());
        if (subscription->second.m_sizeThreshold)
            m_sizeThresholds.erase(subscription->second.m_sizeThreshold.get());

        if (!m_counterThresholds.empty() &&
            *m_counterThresholds.begin() != m_counterThreshold.get())
            m_counterThreshold.reset(*m_counterThresholds.begin());
        if (!m_timeThresholds.empty() &&
            *m_timeThresholds.begin() != m_timeThreshold.get())
            m_timeThreshold.reset(*m_timeThresholds.begin());
        if (!m_sizeThresholds.empty() &&
            *m_sizeThresholds.begin() != m_sizeThreshold.get())
            m_sizeThreshold.reset(*m_sizeThresholds.begin());

        m_subscriptions.erase(subscription);
        return true;
    }
    return false;
}

bool WriteEventStream::isEmissionRuleSatisfied()
{
    auto time = std::chrono::system_clock::now() - m_time;
    return (m_counterThreshold && m_counter >= m_counterThreshold.get()) ||
           (m_timeThreshold && time >= m_timeThreshold.get()) ||
           (m_sizeThreshold && m_size >= m_sizeThreshold.get());
}

void WriteEventStream::emit()
{
    for (const auto &event : m_events)
        m_buffer.lock()->push(std::make_unique<WriteEvent>(event.second));
    m_events.clear();
}

} // namespace events
} // namespace client
} // namespace one
