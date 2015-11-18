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

class ReadSubscription;

/**
 * @c ReadEvent class represents a read operation in the file system.
 */
class ReadEvent : public Event {
public:
    using EventPtr = std::unique_ptr<ReadEvent>;
    using Key = std::string;
    using FileBlock = one::messages::fuse::FileBlock;
    using FileBlocksMap = boost::icl::interval_map<off_t, FileBlock,
        boost::icl::partial_enricher>;
    using Subscription = ReadSubscription;

    /**
     * Constructor.
     * @param offset Distance from the beginning of the file to the first byte
     * read.
     * @param size Number of bytes read.
     * @param fileUuid UUID of a file associated with a read operation.
     * @param storageId ID of a storage where a read operation occurred.
     * @param fileId ID of a file on the storage where a read operation
     * occurred.
     */
    ReadEvent(off_t offset, std::size_t size, std::string fileUuid,
        std::string storageId = {}, std::string fileId = {});

    /**
     * @return Value that distinguish @c this read event from other read events,
     * i.e. read events with the same key can be aggregated.
     * @see @c ReadEvent::Key.
     */
    const Key &key() const;

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
    const FileBlocksMap &blocks() const;

    /**
     * Aggregates @c this event with an other read event.
     * Aggregation is done by:
     * - addition of events' counters
     * - addition of events' sizes
     * - union of sets of read blocks
     * @param event Read event to be aggregated.
     */
    void aggregate(EventPtr event);

    std::string toString() const override;

    std::unique_ptr<ProtocolEventMessage>
    serialize() const override;

protected:
    std::string m_fileUuid;
    std::size_t m_size = 0;
    FileBlocksMap m_blocks;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_READ_EVENT_H
