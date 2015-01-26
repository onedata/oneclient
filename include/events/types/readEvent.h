/**
* @file readEvent.h
* @author Krzysztof Trzepla
* @copyright (C) 2015 ACK CYFRONET AGH
* @copyright This software is released under the MIT license cited in
* 'LICENSE.txt'
*/

#ifndef ONECLIENT_EVENTS_TYPES_READ_EVENT_H
#define ONECLIENT_EVENTS_TYPES_READ_EVENT_H

#include "event.h"

#include <boost/optional.hpp>
#include <boost/icl/interval_set.hpp>

#include <chrono>
#include <memory>

namespace one {
namespace client {
namespace events {

class ReadEventStream;
class ReadEventSerializer;
class ReadEventSubscription;

class ReadEvent : public Event {
    friend class ReadEventStream;
    friend class ReadEventSerializer;

public:
    ReadEvent(std::string fileId, off_t offset, size_t size);

    virtual ~ReadEvent() = default;

    ReadEvent &operator+=(const ReadEvent &event);

    virtual Type type() const override;

    virtual std::unique_ptr<EventSerializer> serializer() const override;

protected:
    std::string m_fileId;
    size_t m_size;
    boost::icl::interval_set<off_t> m_blocks;
};

class ReadEventSerializer : public EventSerializer {
public:
    virtual std::unique_ptr<google::protobuf::Message>
    serialize(long long id, const Event &event) const override;
};

class ReadEventStream : public EventStream {
public:
    ReadEventStream(const ReadEventSubscription &subscription);

    ~ReadEventStream() = default;

    ReadEventStream &operator+=(const ReadEventStream &stream);

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
};

} // namespace events
} // namespace client
} // namespace one

#endif