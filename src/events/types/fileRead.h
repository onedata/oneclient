/**
 * @file fileRead.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_READ_H
#define ONECLIENT_EVENTS_TYPES_FILE_READ_H

#include "messages/fuse/fileBlock.h"
#include "remoteEvent.h"

#include <boost/icl/interval_map.hpp>

namespace one {
namespace client {
namespace events {

/**
 * @c FileRead class represents a read file operation in the system.
 */
class FileRead : public RemoteEvent {
    using FileBlock = one::messages::fuse::FileBlock;
    using FileBlocksMap = boost::icl::interval_map<off_t, FileBlock,
        boost::icl::partial_enricher>;

public:
    /**
     * Constructor.
     * @param fileUuid UUID of a file associated with the read operation.
     * @param offset Distance from the beginning of the file to the first byte
     * read.
     * @param size Number of bytes read.
     * @param storageId ID of a storage where the read operation occurred.
     * @param fileId ID of a file on the storage where the read operation
     * occurred.
     */
    FileRead(std::string fileUuid, off_t offset, std::size_t size,
        std::string storageId = {}, std::string fileId = {});

    StreamKey streamKey() const override;

    /**
     * Aggregation key value is equal to the UUID of a file associated with the
     * event.
     * @see Event::aggregationKey()
     */
    const AggregationKey &aggregationKey() const override;

    std::string toString() const override;

    /**
     * Aggregates @c *this event with the other event. Aggregation is done by
     * addition of events' counters and sizes and union of read blocks.
     * @param event An event to be aggregated.
     */
    void aggregate(EventPtr<FileRead> event);

    ProtoEventPtr serializeAndDestroy() override;

private:
    std::size_t m_counter = 1;
    std::string m_fileUuid;
    std::size_t m_size = 0;
    FileBlocksMap m_blocks;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_READ_H
