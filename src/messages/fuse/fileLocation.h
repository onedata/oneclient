/**
 * @file fileLocation.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_LOCATION_H
#define ONECLIENT_MESSAGES_FUSE_FILE_LOCATION_H

#include "fileBlock.h"
#include "fuseResponse.h"

#include <boost/icl/interval_map.hpp>

#include <sys/types.h>

#include <memory>
#include <set>
#include <string>
#include <unordered_map>

namespace one {
namespace messages {
namespace fuse {

/**
 * The @c FileLocation class represents server-sent information about file
 * location.
 */
class FileLocation : public FuseResponse {
public:
    FileLocation() = default;

    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileLocation counterpart.
     */
    FileLocation(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return File UUID.
     */
    const std::string &uuid() const { return m_uuid; }

    /**
     * @return Default storage ID of the file.
     */
    const std::string &storageId() const { return m_storageId; }

    /**
     * @return File ID on the default storage id.
     */
    const std::string &fileId() const { return m_fileId; }

    /**
     * @return Blocks per storageId/fileId pair.
     */
    const auto &blocks() const { return m_blocks; }

    std::string toString() const override;

private:
    std::string m_uuid;
    std::string m_storageId;
    std::string m_fileId;
    boost::icl::interval_map<off_t, FileBlock> m_blocks;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_LOCATION_H
