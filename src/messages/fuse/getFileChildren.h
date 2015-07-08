/**
 * @file getFileChildren.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_FUSE_MESSAGES_GET_FILE_CHILDREN_H
#define HELPERS_MESSAGES_FUSE_MESSAGES_GET_FILE_CHILDREN_H

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
     * @param uuid
     */
    GetFileChildren(std::string uuid);

    /**
     * Constructor.
     * @param uuid
     * @param offset
     * @param size
     */
    GetFileChildren(std::string uuid, off_t offset, std::size_t size);

    std::string toString() const override;

    std::unique_ptr<ProtocolClientMessage> serialize() const override;

private:
    std::string m_uuid;
    boost::optional<off_t> m_offset;
    boost::optional<std::size_t> m_size;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_FUSE_MESSAGES_GET_FILE_CHILDREN_H
