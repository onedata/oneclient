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
#include "events/messages/readEventSubscription.h"

#include <boost/optional.hpp>
#include <boost/icl/interval_set.hpp>

#include <map>
#include <set>
#include <chrono>
#include <memory>

namespace one {
namespace client {

class Context;

namespace events {

class EventBuffer;
class ReadEventStream;
class ReadEventSerializer;

class ReadEvent : public Event {
    friend class ReadEventStream;
    friend class ReadEventSerializer;

public:
    ReadEvent(std::string fileId, off_t offset, size_t size,
              std::weak_ptr<ReadEventStream> stream);

    virtual ~ReadEvent() = default;

    ReadEvent &operator+=(const ReadEvent &event);

    friend std::ostream &operator<<(std::ostream &, const ReadEvent &event);

    virtual void emit() override;

    virtual std::unique_ptr<EventSerializer> serializer() const override;

protected:
    std::string m_fileId;
    size_t m_size;
    boost::icl::interval_set<off_t> m_blocks;
    std::weak_ptr<ReadEventStream> m_stream;
};

class ReadEventSerializer : public EventSerializer {
public:
    virtual ~ReadEventSerializer() = default;

    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber,
              const Event &event) const override;
};

class ReadEventStream {
public:
    ReadEventStream(std::weak_ptr<Context> context,
                    std::weak_ptr<EventBuffer> buffer);

    void push(const ReadEvent &event);

    unsigned long long subscribe(const ReadEventSubscription &subscription);

    bool cancelSubscription(unsigned long long id);

private:
    bool isEmissionRuleSatisfied();

    void emit();

    size_t m_counter;
    boost::optional<size_t> m_counterThreshold;
    std::chrono::time_point<std::chrono::system_clock> m_time;
    boost::optional<std::chrono::milliseconds> m_timeThreshold;
    size_t m_size;
    boost::optional<size_t> m_sizeThreshold;
    std::weak_ptr<Context> m_context;
    std::weak_ptr<EventBuffer> m_buffer;
    std::map<std::string, ReadEvent> m_events;
    std::map<unsigned long long, ReadEventSubscription> m_subscriptions;
    std::multiset<size_t> m_counterThresholds;
    std::multiset<std::chrono::milliseconds> m_timeThresholds;
    std::multiset<size_t> m_sizeThresholds;
};

} // namespace events
} // namespace client
} // namespace one

#endif