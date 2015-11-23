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

#include "messages/fuse/fileBlock.h"

#include <boost/icl/interval_map.hpp>

#include <sys/types.h>

#include <string>
#include <memory>

namespace one {
namespace client {
namespace events {

class ReadEventSubscription;

/**
 * The ReadEvent class represents a read operation in the file system.
 */
class ReadEvent : public Event {
public:
    typedef typename one::client::events::ReadEventSubscription Subscription;
    using FileBlock = one::messages::fuse::FileBlock;

    /**
     * Default constructor.
     * Creates identity element for read events aggregation operation.
     */
    ReadEvent();

    /**
     * Constructor.
     * @param fileUuid UUID of file associated with a read operation.
     * @param offset Distance from the beginning of the file to the first byte
     * read.
     * @param storageId ID of a storage the write has been written to,
     * @param fileId ID of a file on the storage.
     * @param size Number of read bytes.
     * @param counter Number of read events aggregated in @c this event.
     */
    ReadEvent(off_t offset, std::size_t size, std::string fileUuid,
        std::size_t counter = 1, std::string storageId = {},
        std::string fileId = {});

    /**
     * @return ID of file associated with the read event.
     */
    const std::string &fileUuid() const;

    /**
     * @return Total number of bytes read.
     */
    std::size_t size() const;

    /**
     * @return Set of bytes blocks read.
     */
    const auto &blocks() const { return m_blocks; }

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

    std::string toString() const override;

private:
    std::unique_ptr<one::messages::ProtocolClientMessage>
    serializeAndDestroy() override;

    std::string m_fileUuid;
    std::size_t m_size = 0;
    boost::icl::interval_map<off_t, FileBlock, boost::icl::partial_enricher>
        m_blocks;
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
