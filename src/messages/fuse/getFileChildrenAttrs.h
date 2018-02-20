/**
 * @file getFileChildrenAttrs.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_GET_FILE_CHILDREN_ATTRS_H
#define ONECLIENT_MESSAGES_FUSE_GET_FILE_CHILDREN_ATTRS_H

#include "fileRequest.h"

#include <folly/FBString.h>
#include <folly/Optional.h>

#include <sys/types.h>

#include <cstdint>
#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The GetFileChildrenAttrs class represents a FUSE request for file children.
 */
class GetFileChildrenAttrs : public FileRequest {
public:
    /**
     * @copydoc GetFileChildrenAttrs(std::string)
     * @param offset A number of skipped entries at the beginning of directory's
     * children list.
     * @param size A number of returned entries of directory's children list.
     */
    GetFileChildrenAttrs(const folly::fbstring &uuid, const off_t offset,
        const std::size_t size);

    /**
     * @copydoc GetFileChildrenAttrs(std::string)
     * @param offset A number of skipped entries at the beginning of directory's
     * children list.
     * @param size A number of returned entries of directory's children list.
     * @param indexToken Token which can be used by the server to find last
     * returned item.
     */
    GetFileChildrenAttrs(const folly::fbstring &uuid, const off_t offset,
        const std::size_t size,
        const folly::Optional<folly::fbstring> &indexToken);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    const off_t m_offset;
    const std::size_t m_size;
    folly::Optional<folly::fbstring> m_indexToken;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_FILE_CHILDREN_ATTRS_H
