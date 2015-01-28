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
#include <mutex>
#include <chrono>
#include <memory>
#include <functional>

namespace one {
namespace client {

class Context;

namespace events {

class EventBuffer;
class ReadEventStream;
class ReadEventSerializer;

/**
* The ReadEvent class represents a read operation in the file system.
*/
class ReadEvent : public Event {
    friend class ReadEventStream;
    friend class ReadEventSerializer;
    friend std::ostream &operator<<(std::ostream &, const ReadEvent &event);

public:
    /**
    * Constructor.
    * @param fileId ID of file associated with a read operation.
    * @param offset Distance from the beginning of the file to the first byte
    * read.
    * @param size Amount of bytes read.
    * @param stream A @c ReadEventStream instance.
    */
    ReadEvent(std::string fileId, off_t offset, size_t size,
              std::weak_ptr<ReadEventStream> stream);

    virtual ~ReadEvent() = default;

    /**
    * Aggregates this read event with the other event.
    * Aggregation is done by:
    * - addition of events counters
    * - addition of events sizes
    * - union of sets of read segments
    * @param event Read event to be aggregated.
    * @return Returns aggregated event.
    */
    ReadEvent &operator+=(const ReadEvent &event);

    /**
    * Emits an @c ReadEvent by pushing it to an @c ReadEventStream.
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
    std::weak_ptr<ReadEventStream> m_stream;
};

/**
* The ReadEventSerializer class is responsible for serialization of the
* @c ReadEvent objects.
*/
class ReadEventSerializer : public EventSerializer {
public:
    virtual ~ReadEventSerializer() = default;

    /**
    * @copydoc EventSerializer::serialize(unsigned long long, const Event &)
    */
    virtual std::unique_ptr<google::protobuf::Message>
    serialize(unsigned long long sequenceNumber,
              const Event &event) const override;
};

/**
* The ReadEventStream class is responsible aggregation and emission of read
* events.
*/
class ReadEventStream {
public:
    /**
    * Constructor.
    * @param context An @c Context instance.
    * @parma buffer An @c EventBuffer instance.
    */
    ReadEventStream(std::weak_ptr<Context> context,
                    std::weak_ptr<EventBuffer> buffer);

    /**
    * Pushes read event to the stream.
    * @param event A @c ReadEvent to be pushed to the @c ReadEventStream.
    */
    void push(const ReadEvent &event);

    /**
    * Adds subscription for read events.
    * @param subscription A @c ReadEventSubscription instance.
    * @return Return subscription ID.
    */
    unsigned long long subscribe(const ReadEventSubscription &subscription);

    /**
    * Cancels given subscription for read events.
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

    std::map<std::string, ReadEvent> m_events;
    std::map<unsigned long long, ReadEventSubscription> m_subscriptions;

    std::mutex m_streamMutex;
    std::function<void()> m_periodicEmissionCancellation = [] {};
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_READ_EVENT_H