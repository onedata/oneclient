/**
* @file writeEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "events/eventBuffer.h"
#include "events/types/writeEvent.h"
#include "events/messages/writeEventSubscription.h"

namespace one {
namespace client {
namespace events {

WriteEvent::WriteEvent(std::string fileId, off_t offset, size_t size,
                       off_t fileSize, size_t counter)
    : Event{counter}
    , m_fileId{std::move(fileId)}
    , m_size{size}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(offset,
                                                                offset + size)}
    , m_fileSize{fileSize}
{
}

WriteEvent &WriteEvent::operator+=(const WriteEvent &event)
{
    m_counter += event.m_counter;
    m_size += event.m_size;
    m_blocks += event.m_blocks;
    m_fileSize = event.m_fileSize;
    return *this;
}

Event::Type WriteEvent::type() const { return Event::Type::WRITE; }

std::unique_ptr<EventSerializer> WriteEvent::serializer() const
{
    return std::make_unique<WriteEventSerializer>();
}

std::unique_ptr<google::protobuf::Message>
WriteEventSerializer::serialize(unsigned long long id, const Event &event) const
{
    auto writeEvent = static_cast<const WriteEvent &>(event);
    auto message = std::make_unique<one::clproto::events::WriteEvent>();
    message->set_id(id);
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

WriteEventStream::WriteEventStream(const WriteEventSubscription &subscription,
                                   std::weak_ptr<Context> context,
                                   std::weak_ptr<EventBuffer> buffer)
    : EventStream(subscription.id())
    , m_counter{0}
    , m_counterThreshold{subscription.counterThreshold()}
    , m_time{}
    , m_timeThreshold{subscription.timeThreshold()}
    , m_size{0}
    , m_sizeThreshold{subscription.sizeThreshold()}
    , m_context{std::move(context)}
    , m_buffer{std::move(buffer)}
{
}

WriteEventStream &WriteEventStream::operator+=(const WriteEventStream &stream)
{
    bool isTimeThresholdUpdated = false;
    if (stream.m_counterThreshold &&
        (!m_counterThreshold ||
         m_counterThreshold.get() > stream.m_counterThreshold.get()))
        m_counterThreshold.reset(stream.m_counterThreshold.get());
    if (stream.m_timeThreshold &&
        (!m_timeThreshold ||
         m_timeThreshold.get() > stream.m_timeThreshold.get())) {
        m_timeThreshold.reset(stream.m_timeThreshold.get());
        isTimeThresholdUpdated = true;
    }
    if (stream.m_sizeThreshold &&
        (!m_sizeThreshold ||
         m_sizeThreshold.get() > stream.m_sizeThreshold.get()))
        m_sizeThreshold.reset(stream.m_sizeThreshold.get());
    if (isTimeThresholdUpdated || isEmissionRuleSatisfied())
        emit();
    return *this;
}

Event::Type WriteEventStream::type() const { return Event::Type::WRITE; }

void WriteEventStream::add(const Event &event) {}

void WriteEventStream::emit() {}

bool WriteEventStream::isEmissionRuleSatisfied()
{
    auto time = std::chrono::system_clock::now() - m_time;
    return (m_counterThreshold && m_counter >= m_counterThreshold.get()) ||
           (m_timeThreshold && time >= m_timeThreshold.get()) ||
           (m_sizeThreshold && m_size >= m_sizeThreshold.get());
}

} // namespace events
} // namespace client
} // namespace one
