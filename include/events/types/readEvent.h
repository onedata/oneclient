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

#include <boost/icl/interval_set.hpp>

#include <string>
#include <memory>
#include <sys/types.h>

namespace one {
namespace client {
namespace events {

class ReadEventSubscription;
template <class EventType> class EventStream;

/**
* The ReadEvent class represents a read operation in the file system.
*/
class ReadEvent : public Event {
public:
    typedef typename one::client::events::ReadEventSubscription subscription;

    /**
    * Default constructor.
    * Creates identity element for read events aggregation operation.
    */
    ReadEvent();

    /**
    * Constructor.
    * @param eventStream Weak pointer to @c ReadEventStream to which this event
    * will be pushed when emitted.
    * @param fileId ID of file associated with a read operation.
    * @param offset Distance from the beginning of the file to the first
    * byte read.
    * @param size Number of read bytes.
    */
    ReadEvent(std::string fileId, off_t offset, size_t size);

    virtual std::string toString() const override;

    /**
    * @return ID of file associated with the read event.
    */
    const std::string &fileId() const;

    /**
    * @return Total number of bytes read.
    */
    size_t size() const;

    /**
    * @return Set of bytes blocks read.
    */
    const boost::icl::interval_set<off_t> &blocks() const;

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

    virtual std::unique_ptr<one::messages::ProtocolClientMessage>
    serialize() const override;

private:
    std::string m_fileId;
    size_t m_size = 0;
    boost::icl::interval_set<off_t> m_blocks;
};

/**
* Compares two read events.
* Read events are equal if corresponding event's fields are equal.
* @param lhs Read event to be compared.
* @param rhs Read event to be compared.
* @return true if read events are equal and false otherwise.
*/
bool operator==(const ReadEvent &lhs, const ReadEvent &rhs);

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_READ_EVENT_H