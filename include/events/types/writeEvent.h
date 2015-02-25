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
#include <sys/types.h>

namespace one {
namespace client {

class WriteEventSubscription;

namespace events {

class WriteEventSerializer;

/**
* The WriteEvent class represents a write operation in the file system.
*/
class WriteEvent : public Event {
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
    * @param size Number of bytes written.
    * @param fileSize Size of file after a write operation.
    */
    WriteEvent(std::string fileId, off_t offset, size_t size, off_t fileSize);

    /**
    * Returns ID of file associated with the write event.
    * @return File ID.
    */
    const std::string &fileId() const;

    /**
    * Returns the total number of bytes written.
    * @return Number of bytes written.
    */
    size_t size() const;

    /**
    * Returns size of file associated with the write event.
    * @return File size.
    */
    off_t fileSize() const;

    /**
    * Returns the set of bytes blocks written.
    * @return Set of bytes blocks written.
    */
    const boost::icl::interval_set<off_t> &blocks() const;

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
    * Returns a @c ClientMessageSerializer instance for the @c ClientMessage.
    * @return Unique pointer to a @ClientMessageSerializer instance.
    */
    virtual std::unique_ptr<ClientMessageSerializer>
    createSerializer() const override;

private:
    std::string m_fileId;
    size_t m_size = 0;
    off_t m_fileSize = 0;
    boost::icl::interval_set<off_t> m_blocks;
};

/**
* Compares two write events.
* Write events are equal if corresponding event's fields are equal.
* @param lhs Write event to be compared.
* @param rhs Write event to be compared.
* @return true if write events are equal and false otherwise.
*/
bool operator==(const WriteEvent &lhs, const WriteEvent &rhs);

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H