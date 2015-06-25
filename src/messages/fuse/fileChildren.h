/**
 * @file fileChildren.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef HELPERS_MESSAGES_FUSE_MESSAGES_FILE_CHILDREN_H
#define HELPERS_MESSAGES_FUSE_MESSAGES_FILE_CHILDREN_H

#include "messages/serverMessage.h"

#include <memory>
#include <string>
#include <vector>

namespace one {

namespace clproto {
class FileChildren;
}

namespace messages {
namespace fuse {

/**
 * The FileChildren class represents server-sent file children list.
 */
class FileChildren : public ServerMessage {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileChildren counterpart.
     */
    FileChildren(std::unique_ptr<ProtocolServerMessage> serverMessage);

    const std::vector<std::string> &uuids() const;

    std::string toString() const override;

private:
    std::vector<std::string> m_uuids;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // HELPERS_MESSAGES_FUSE_MESSAGES_FILE_CHILDREN_H
