/**
 * @file getFileChildren.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_GET_FILE_CHILDREN_H
#define ONECLIENT_MESSAGES_FUSE_GET_FILE_CHILDREN_H

#include "messages/clientMessage.h"

#include <boost/optional.hpp>

#include <sys/types.h>

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The GetFileChildren class represents a FUSE request for file children.
 */
class GetFileChildren : public ClientMessage {
public:
    /**
     * Constructor.
     * @param uuid UUID of the directory of which children are requested.
     */
    GetFileChildren(std::string uuid);

    /**
     * @copydoc GetFileChildren(std::string)
     * @param offset A number of skipped entries at the beginning of directory's
     * children list.
     * @param size A number of returned entries of directory's children list.
     */
    GetFileChildren(std::string uuid, off_t offset, std::size_t size);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    std::string m_uuid;
    boost::optional<off_t> m_offset;
    boost::optional<std::size_t> m_size;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_FILE_CHILDREN_H
