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
#include "messages/client/clientMessage.h"

#include <boost/icl/interval_set.hpp>

#include <string>
#include <memory>
#include <ostream>

namespace one {
namespace client {

class ReadEventSubscription;

namespace events {

class ReadEventSerializer;
template <class EventType, class SubscriptionType> class EventStream;

/**
* The ReadEvent class represents a read operation in the file system.
*/
class ReadEvent : public Event, public ClientMessage {
    friend class EventStream<ReadEvent, one::client::ReadEventSubscription>;
    friend class ReadEventSerializer;
    friend std::ostream &operator<<(std::ostream &, const ReadEvent &event);

public:
    /**
    * Default constructor.
    * Creates identity element for read events aggregation operation.
    */
    ReadEvent();

    /**
    * Constructor.
    * @param fileId ID of file associated with a read operation.
    * @param offset Distance from the beginning of the file to the first
    * byte read.
    * @param size Amount of bytes read.
    */
    ReadEvent(std::string fileId, off_t offset, size_t size);

    /**
    * Aggregates this read event with an other read event.
    * Aggregation is done by:
    * - addition of events counters
    * - addition of events sizes
    * - union of sets of read segments
    * @param event Read event to be aggregated.
    * @return @c *this
    */
    ReadEvent &operator+=(const ReadEvent &event);

    /**
    * Returns ID of file associated with the read event.
    * @return File ID.
    */
    const std::string &fileId() const;

    /**
    * Returns a @c ClientMessageSerializer instance for the @c ClientMessage.
    * @return Unique pointer to a @ClientMessageSerializer instance.
    */
    virtual std::unique_ptr<ClientMessageSerializer>
    createSerializer() const override;

private:
    std::string m_fileId;
    size_t m_size;
    boost::icl::interval_set<off_t> m_blocks;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_READ_EVENT_H