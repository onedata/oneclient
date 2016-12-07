/**
 * @file fileWritten.h
 * @author Krzysztof Trzepla
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_EVENTS_TYPES_FILE_WRITTEN_H
#define ONECLIENT_EVENTS_TYPES_FILE_WRITTEN_H

#include "messages/fuse/fileBlock.h"
#include "remoteEvent.h"

#include <boost/icl/interval_map.hpp>
#include <boost/optional.hpp>

namespace one {
namespace client {
namespace events {

class FileWritten : public RemoteEvent {
    using FileBlock = one::messages::fuse::FileBlock;
    using FileBlocksMap = boost::icl::interval_map<off_t, FileBlock,
        boost::icl::partial_enricher>;

public:
    /**
     * Constructor.
     * @param fileUuid UUID of a file associated with a write operation.
     * @param offset Distance from the beginning of the file to the first byte
     * written.
     * @param size Number of bytes read.
     * @param storageId ID of a storage where a write operation occurred.
     * @param fileId ID of a file on the storage where a write operation
     * occurred.
     * @param fileSize Size of a file after write operation.
     */
    FileWritten(std::string fileUuid, off_t offset, std::size_t size,
        std::string storageId = {}, std::string fileId = {},
        boost::optional<off_t> fileSize = {});

    StreamKey streamKey() const override;

    const std::string &aggregationKey() const override;

    std::string toString() const override;

    void aggregate(EventPtr<FileWritten> event);

    ProtoEventPtr serializeAndDestroy() override;

protected:
    std::size_t m_counter = 1;
    std::string m_fileUuid;
    std::size_t m_size = 0;
    FileBlocksMap m_blocks;
    boost::optional<off_t> m_fileSize;
};

} // namespace events
} // namespace client
} // namespace one

#endif // ONECLIENT_EVENTS_TYPES_FILE_WRITTEN_H
