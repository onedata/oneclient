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
#include "messages/client/clientMessage.h"

#include <boost/icl/interval_set.hpp>

#include <string>
#include <memory>
#include <ostream>

namespace one {
namespace client {

class WriteEventSubscription;

namespace events {

class WriteEventSerializer;
template <class EventType, class SubscriptionType> class EventStream;

/**
* The WriteEvent class represents a write operation in the file system.
*/
class WriteEvent : public Event, public ClientMessage {
    friend class EventStream<WriteEvent, one::client::WriteEventSubscription>;
    friend class WriteEventSerializer;
    friend std::ostream &operator<<(std::ostream &, const WriteEvent &event);

public:
    /**
    * Default constructor.
    * Creates identity element for write events aggregation operation.
    */
    WriteEvent();

    /**
    * Constructor.
    * @param fileId ID of file associated with a write operation.
    * @param offset Distance from the beginning of the file to the first
    * byte written.
    * @param size Amount of bytes written.
    * @param fileSize Size of file after a write operation.
    */
    WriteEvent(std::string fileId, off_t offset, size_t size, off_t fileSize);

    /**
    * Aggregates this write event with an other write event.
    * Aggregation is done by:
    * - addition of events counters
    * - addition of events sizes
    * - union of sets of write segments
    * @param event Write event to be aggregated.
    * @return @c *this
    */
    WriteEvent &operator+=(const WriteEvent &event);

    /**
    * Returns ID of file associated with the write event.
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
    off_t m_fileSize;
    boost::icl::interval_set<off_t> m_blocks;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H