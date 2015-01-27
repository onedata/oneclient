/**
* @file readEvent.cc
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#include "context.h"

#include "events.pb.h"
#include "communication_protocol.pb.h"

#include "events/eventBuffer.h"
#include "events/types/readEvent.h"
#include "events/messages/readEventSubscription.h"

namespace one {
namespace client {
namespace events {

ReadEvent::ReadEvent(std::string fileId, off_t offset, size_t size,
                     size_t counter)
    : Event{counter}
    , m_fileId{std::move(fileId)}
    , m_size{size}
    , m_blocks{boost::icl::discrete_interval<off_t>::right_open(offset,
                                                                offset + size)}
{
}

ReadEvent &ReadEvent::operator+=(const ReadEvent &event)
{
    m_counter += event.m_counter;
    m_size += event.m_size;
    m_blocks += event.m_blocks;
    return *this;
}

Event::Type ReadEvent::type() const { return Event::Type::READ; }

std::unique_ptr<EventSerializer> ReadEvent::serializer() const
{
    return std::make_unique<ReadEventSerializer>();
}

std::unique_ptr<google::protobuf::Message>
ReadEventSerializer::serialize(unsigned long long id, const Event &event) const
{
    auto readEvent = static_cast<const ReadEvent &>(event);
    auto message = std::make_unique<one::clproto::events::ReadEvent>();
    message->set_id(id);
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

ReadEventStream::ReadEventStream(const ReadEventSubscription &subscription,
                                 std::weak_ptr<Context> context,
                                 std::weak_ptr<EventBuffer> buffer)
    : EventStream{subscription.id()}
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

ReadEventStream &ReadEventStream::operator+=(const ReadEventStream &stream)
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

Event::Type ReadEventStream::type() const { return Event::Type::READ; }

void ReadEventStream::add(const Event &event) {}

void ReadEventStream::emit() {}

bool ReadEventStream::isEmissionRuleSatisfied()
{
    auto time = std::chrono::system_clock::now() - m_time;
    return (m_counterThreshold && m_counter >= m_counterThreshold.get()) ||
           (m_timeThreshold && time >= m_timeThreshold.get()) ||
           (m_sizeThreshold && m_size >= m_sizeThreshold.get());
}

} // namespace events
} // namespace client
} // namespace one
