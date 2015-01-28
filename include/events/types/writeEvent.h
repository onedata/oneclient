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

/**
* The WriteEvent class represents a write operation in the file system.
*/
class WriteEvent : public Event {
    friend class WriteEventStream;
    friend class WriteEventSerializer;
    friend std::ostream &operator<<(std::ostream &, const WriteEvent &event);

public:
    /**
    * Constructor.
    * @param fileId ID of file associated with a write operation.
    * @param offset Distance from the beginning of the file to the first byte
    * write.
    * @param size Amount of bytes write.
    * @param fileSize Size of file associated with a write operation.
    * @param stream A @c WriteEventStream instance.
    */
    WriteEvent(std::string fileId, off_t offset, size_t size, off_t fileSize,
               std::weak_ptr<WriteEventStream> stream);

    virtual ~WriteEvent() = default;

    /**
    * Aggregates this write event with the other event.
    * Aggregation is done by:
    * - addition of events counters
    * - addition of events sizes
    * - union of sets of write segments
    * - substitution of other event file size
    * @param event Write event to be aggregated.
    * @return Returns aggregated event.
    */
    WriteEvent &operator+=(const WriteEvent &event);

    /**
    * Emits an @c WriteEvent by pushing it to an @c WriteEventStream.
    */
    virtual void emit() override;

    /**
    * @copydoc Event::serializer()
    */
    virtual std::unique_ptr<EventSerializer> serializer() const override;

protected:
    std::string m_fileId;
    size_t m_size;
    boost::icl::interval_set<off_t> m_blocks;
    off_t m_fileSize;
    std::weak_ptr<WriteEventStream> m_stream;
};

/**
* The WriteEventSerializer class is responsible for serialization of the
* @c WriteEvent objects.
*/
class WriteEventSerializer : public EventSerializer {
public:
    virtual ~WriteEventSerializer() = default;

    /**
    * @copydoc EventSerializer::serialize(unsigned long long, const Event &)
    */
    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber,
              const Event &event) const override;
};

/**
* The WriteEventStream class is responsible aggregation and emission of write
* events.
*/
class WriteEventStream {
public:
    /**
    * Constructor.
    * @param context An @c Context instance.
    * @parma buffer An @c EventBuffer instance.
    */
    WriteEventStream(std::weak_ptr<Context> context,
                     std::weak_ptr<EventBuffer> buffer);

    /**
    * Pushes write event to the stream.
    * @param event A @c WriteEvent to be pushed to the @c WriteEventStream.
    */
    void push(const WriteEvent &event);

    /**
    * Adds subscription for write events.
    * @param subscription A @c WriteEventSubscription instance.
    * @return Return subscription ID.
    */
    unsigned long long subscribe(const WriteEventSubscription &subscription);

    /**
    * Cancels given subscription for write events.
    * @param id ID of subscription to be cancelled.
    * @return Returns @c true in case of successful subscription cancellation or
    * @c false otherwise.
    */
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