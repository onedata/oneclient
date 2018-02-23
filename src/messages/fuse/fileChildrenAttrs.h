/**
 * @file fileChildrenAttrs.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_ATTRS_H
#define ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_ATTRS_H

#include "fileAttr.h"
#include "fuseResponse.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Optional.h>

#include <memory>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The FileChildrenAttrs class represents server-sent file children list with
 * attributes.
 */
class FileChildrenAttrs : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileChildrenAttrs counterpart.
     */
    FileChildrenAttrs(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return A list of directory's children, specified by their UUID and
     * filename.
     */
    const folly::fbvector<FileAttr> &childrenAttrs() const
    {
        return m_childrenAttrs;
    }

    /**
     * @return Optional index token which contains id of the last returned
     *         item.
     */
    folly::Optional<folly::fbstring> indexToken() const { return m_indexToken; }

    /**
     * @return Optional flag determining whether there are any more children
     *                  in directory.
     */
    folly::Optional<bool> isLast() const { return m_isLast; }

    std::string toString() const override;

private:
    folly::fbvector<FileAttr> m_childrenAttrs;

    folly::Optional<folly::fbstring> m_indexToken;

    folly::Optional<bool> m_isLast;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_CHILDREN_ATTRS_H
