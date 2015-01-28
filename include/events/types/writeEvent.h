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
#include "events/messages/writeEventSubscription.h"

#include <boost/optional.hpp>
#include <boost/icl/interval_set.hpp>

#include <map>
#include <set>
#include <chrono>
#include <memory>
#include <functional>

namespace one {
namespace client {

class Context;

namespace events {

class EventBuffer;
class WriteEventStream;
class WriteEventSerializer;

class WriteEvent : public Event {
    friend class WriteEventStream;
    friend class WriteEventSerializer;

public:
    WriteEvent(std::string fileId, off_t offset, size_t size, off_t fileSize,
               std::weak_ptr<WriteEventStream> stream);

    virtual ~WriteEvent() = default;

    friend std::ostream &operator<<(std::ostream &, const WriteEvent &event);

    WriteEvent &operator+=(const WriteEvent &event);

    virtual void emit() override;

    virtual std::unique_ptr<EventSerializer> serializer() const override;

protected:
    std::string m_fileId;
    size_t m_size;
    boost::icl::interval_set<off_t> m_blocks;
    off_t m_fileSize;
    std::weak_ptr<WriteEventStream> m_stream;
};

class WriteEventSerializer : public EventSerializer {
public:
    virtual ~WriteEventSerializer() = default;

    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber,
              const Event &event) const override;
};

class WriteEventStream {
public:
    WriteEventStream(std::weak_ptr<Context> context,
                     std::weak_ptr<EventBuffer> buffer);

    void push(const WriteEvent &event);

    unsigned long long subscribe(const WriteEventSubscription &subscription);

    bool cancelSubscription(unsigned long long id);

private:
    bool isEmissionRuleSatisfied();
    void emit();
    void periodicEmit();
    void resetStatistics();

    size_t m_counter = 0;
    boost::optional<size_t> m_counterThreshold;
    std::multiset<size_t> m_counterThresholds;

    boost::optional<std::chrono::milliseconds> m_timeThreshold;
    std::multiset<std::chrono::milliseconds> m_timeThresholds;

    size_t m_size = 0;
    boost::optional<size_t> m_sizeThreshold;
    std::multiset<size_t> m_sizeThresholds;

    std::weak_ptr<Context> m_context;
    std::weak_ptr<EventBuffer> m_buffer;

    std::map<std::string, WriteEvent> m_events;
    std::map<unsigned long long, WriteEventSubscription> m_subscriptions;

    std::mutex m_streamMutex;
    std::function<void()> m_periodicEmissionCancellation = [] {};
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H