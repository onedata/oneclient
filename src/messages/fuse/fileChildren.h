/**
 * @file fileChildren.h
 * @author Konrad Zemek
 * @copyright (C) 2015 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_H
#define ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_H

#include "fuseResponse.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>

#include <memory>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The FileChildren class represents server-sent file children list.
 */
class FileChildren : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileChildren counterpart.
     */
    FileChildren(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return A list of directory's children, specified by their UUID and
     * filename.
     */
    const folly::fbvector<folly::fbstring> &children() const
    {
        return m_children;
    }

    std::string toString() const override;

private:
    folly::fbvector<folly::fbstring> m_children;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_H
