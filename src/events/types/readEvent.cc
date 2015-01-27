/**
* @file readEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"
#include "logging.h"

#include "events.pb.h"

#include "events/eventBuffer.h"
#include "events/types/readEvent.h"

namespace one {
namespace client {
namespace events {

ReadEvent::ReadEvent(std::string fileId, off_t offset, size_t size,
                     std::weak_ptr<ReadEventStream> stream)
    : m_fileId{std::move(fileId)}
    , m_size{size}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(offset,
                                                                offset + size)}
    , m_stream{std::move(stream)}
{
}

ReadEvent &ReadEvent::operator+=(const ReadEvent &event)
{
    m_counter += event.m_counter;
    m_size += event.m_size;
    m_blocks += event.m_blocks;
    return *this;
}

std::ostream &operator<<(std::ostream &ostream, const ReadEvent &event)
{
    return ostream << "type: 'READ', counter: '" << event.m_counter
                   << "', file ID: '" << event.m_fileId << "', size: '"
                   << event.m_size << "', blocks: " << event.m_blocks;
}

void ReadEvent::emit() { m_stream.lock()->push(*this); }

std::unique_ptr<EventSerializer> ReadEvent::serializer() const
{
    return std::make_unique<ReadEventSerializer>();
}

std::unique_ptr<google::protobuf::Message>
ReadEventSerializer::serialize(unsigned long long sequenceNumber,
                               const Event &event) const
{
    auto readEvent = static_cast<const ReadEvent &>(event);
    auto message = std::make_unique<one::clproto::events::ReadEvent>();
    message->set_seq_num(sequenceNumber);
    message->set_counter(readEvent.m_counter);
    message->set_file_id(std::move(readEvent.m_fileId));
    message->set_size(readEvent.m_size);
    for (const auto &block : readEvent.m_blocks) {
        auto blockMessage = message->add_blocks();
        blockMessage->set_offset(block.lower());
        blockMessage->set_size(block.upper() - block.lower());
    }
    return std::move(message);
}

ReadEventStream::ReadEventStream(std::weak_ptr<Context> context,
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

void ReadEventStream::push(const ReadEvent &event)
{
    if (!m_subscriptions.empty()) {
        m_counter += event.m_counter;
        m_size += event.m_size;

        auto readEvent = m_events.find(event.m_fileId);
        if (readEvent != m_events.end())
            readEvent->second += event;
        else
            m_events.insert(std::make_pair(event.m_fileId, event));

        if (isEmissionRuleSatisfied())
            emit();
    }
}

unsigned long long
ReadEventStream::subscribe(const ReadEventSubscription &subscription)
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

bool ReadEventStream::cancelSubscription(unsigned long long id)
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

bool ReadEventStream::isEmissionRuleSatisfied()
{
    auto time = std::chrono::system_clock::now() - m_time;
    return (m_counterThreshold && m_counter >= m_counterThreshold.get()) ||
           (m_timeThreshold && time >= m_timeThreshold.get()) ||
           (m_sizeThreshold && m_size >= m_sizeThreshold.get());
}

void ReadEventStream::emit()
{
    for (const auto &event : m_events) {
        DLOG(INFO) << "Pushing event (" << event.second << ") to buffer.";
        m_buffer.lock()->push(std::make_unique<ReadEvent>(event.second));
    }
    m_events.clear();
}

} // namespace events
} // namespace client
} // namespace one
