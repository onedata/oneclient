/**
* @file writeEvent.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H

#include "event.h"

#include <boost/optional.hpp>
#include <boost/icl/interval_set.hpp>

#include <chrono>
#include <memory>

namespace one {
namespace client {

class Context;

namespace events {

class EventBuffer;
class WriteEventStream;
class WriteEventSerializer;
class WriteEventSubscription;

class WriteEvent : public Event {
    friend class WriteEventStream;
    friend class WriteEventSerializer;

public:
    WriteEvent(std::string fileId, off_t offset, size_t size, off_t fileSize);

    virtual ~WriteEvent() = default;

    WriteEvent &operator+=(const WriteEvent &event);

    virtual Type type() const override;

    virtual std::unique_ptr<EventSerializer> serializer() const override;

protected:
    std::string m_fileId;
    size_t m_size;
    boost::icl::interval_set<off_t> m_blocks;
    off_t m_fileSize;
};

class WriteEventSerializer : public EventSerializer {
public:
    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long id, const Event &event) const override;
};

class WriteEventStream : public EventStream {
public:
    WriteEventStream(const WriteEventSubscription &subscription,
                     std::weak_ptr<Context> context,
                     std::weak_ptr<EventBuffer> buffer);

    ~WriteEventStream() = default;

    WriteEventStream &operator+=(const WriteEventStream &stream);

    virtual Event::Type type() const override;

    virtual void add(const Event &event) override;

    virtual void emit() override;

private:
    bool isEmissionRuleSatisfied();

    size_t m_counter;
    boost::optional<size_t> m_counterThreshold;
    std::chrono::time_point<std::chrono::system_clock> m_time;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
    size_t m_size;
    boost::optional<size_t> m_sizeThreshold;
    std::weak_ptr<Context> m_context;
    std::weak_ptr<EventBuffer> m_buffer;
};

} // namespace events
} // namespace client
} // namespace one

#endif