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

#include <sys/types.h>

#include <string>
#include <memory>

namespace one {

namespace clproto {
class ReadEventSubscription;
}

namespace client {
namespace events {

/**
 * The ReadEvent class represents a read operation in the file system.
 */
class ReadEvent : public Event {
public:
    static const std::string name;

    typedef typename one::clproto::ReadEventSubscription Subscription;

    /**
     * Default constructor.
     * Creates identity element for read events aggregation operation.
     */
    ReadEvent()
        : Event{0}
    {
    }

    /**
     * Constructor.
     * @param fileId ID of file associated with a read operation.
     * @param offset Distance from the beginning of the file to the first byte
     * read.
     * @param size Number of bytes read.
     */
    ReadEvent(std::string fileId, off_t offset, std::size_t size);

    /**
     * @return ID of file associated with the read event.
     */
    const std::string &fileId() const;

    /**
     * @return Total number of bytes read.
     */
    std::size_t size() const;

    /**
     * @return Set of bytes blocks read.
     */
    const boost::icl::interval_set<off_t> &blocks() const;

    /**
     * Aggregates this read event with an other read event.
     * Aggregation is done by:
     * - addition of events' counters
     * - addition of events' sizes
     * - union of sets of read blocks
     * @param event Read event to be aggregated.
     * @return @c *this
     */
    ReadEvent &operator+=(const ReadEvent &evt);

    /**
     * Compares this read event with an other read event.
     * @param evt Read event to be compared.
     * @return 'true' if ID of file associated with this event is
     * lexicographically less than the file ID associated with an other event,
     * otherwise 'false'
     */
    bool operator<(const ReadEvent &evt);

    virtual std::string toString() const override;

    virtual std::unique_ptr<one::messages::ProtocolClientMessage>
    serialize() const override;

private:
    std::string m_fileId;
    std::size_t m_size = 0;
    boost::icl::interval_set<off_t> m_blocks;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_READ_EVENT_H
