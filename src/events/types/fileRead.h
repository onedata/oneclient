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

class FileRead : public RemoteEvent {
    using FileBlock = one::messages::fuse::FileBlock;
    using FileBlocksMap = boost::icl::interval_map<off_t, FileBlock,
        boost::icl::partial_enricher>;

public:
    /**
     * Constructor.
     * @param fileUuid UUID of a file associated with a read operation.
     * @param offset Distance from the beginning of the file to the first byte
     * read.
     * @param size Number of bytes read.
     * @param storageId ID of a storage where a read operation occurred.
     * @param fileId ID of a file on the storage where a read operation
     * occurred.
     */
    FileRead(std::string fileUuid, off_t offset, std::size_t size,
        std::string storageId = {}, std::string fileId = {});

    StreamKey streamKey() const override;

    const std::string &aggregationKey() const override;

    std::string toString() const override;

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
