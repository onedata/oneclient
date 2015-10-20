/**
 * @file writeEvent.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H
#define ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H

#include "clientEvent.h"
#include "messages/fuse/fileBlock.h"

#include <boost/icl/interval_map.hpp>
#include <boost/optional.hpp>

#include <sys/types.h>

#include <string>
#include <memory>

namespace one {
namespace client {
namespace events {

class WriteSubscription;

/**
 * @c WriteEvent class represents a write operation in the file system.
 */
class WriteEvent : public ClientEvent {
public:
    using EventPtr = std::unique_ptr<WriteEvent>;
    using Key = std::string;
    using FileBlock = one::messages::fuse::FileBlock;
    using FileBlocksMap = boost::icl::interval_map<off_t, FileBlock,
        boost::icl::partial_enricher>;
    using Subscription = WriteSubscription;

    /**
     * Constructor.
     * @param offset Distance from the beginning of the file to the first byte
     * written.
     * @param size Number of bytes read.
     * @param fileUuid UUID of a file associated with a write operation.
     * @param fileSize Size of a file after write operation.
     * @param storageId ID of a storage where a write operation occurred.
     * @param fileId ID of a file on the storage where a write operation
     * occurred.
     */
    WriteEvent(off_t offset, std::size_t size, std::string fileUuid,
        std::string storageId = {}, std::string fileId = {});

    /**
     * Constructor.
     * @param offset Distance from the beginning of the file to the first byte
     * written.
     * @param size Number of bytes read.
     * @param fileSize Size of a file after write operation.
     * @param fileUuid UUID of a file associated with a write operation.
     * @param storageId ID of a storage where a write operation occurred.
     * @param fileId ID of a file on the storage where a write operation
     * occurred.
     */
    WriteEvent(off_t offset, std::size_t size, off_t fileSize,
        std::string fileUuid, std::string storageId = {},
        std::string fileId = {});

    /**
     * @return Value that distinguish @c this write event from other write
     * events, i.e. write events with the same key can be aggregated.
     * @see @c WriteEvent::Key.
     */
    const Key &key() const;

    /**
     * @return UUID of file associated with the write event.
     */
    const std::string &fileUuid() const;

    /**
     * @return Number of bytes written.
     */
    std::size_t size() const;

    /**
     * @return Size of file associated with the write event.
     */
    boost::optional<off_t> fileSize() const;

    /**
     * @return Set of bytes blocks written.
     */
    const FileBlocksMap &blocks() const;

    /**
     * Aggregates @c this event with an other write event.
     * Aggregation is done by:
     * - addition of events' counters
     * - addition of events' sizes
     * - union of sets of write blocks
     * - substitution of file size with file size associated with other event
     * @param event Write event to be aggregated.
     */
    void aggregate(EventPtr event);

    std::string toString() const override;

    std::unique_ptr<one::messages::ProtocolClientMessage>
    serialize() const override;

protected:
    std::string m_fileUuid;
    std::size_t m_size = 0;
    boost::optional<off_t> m_fileSize;
    FileBlocksMap m_blocks;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_WRITE_EVENT_H
