/**
 * @file listFilesRecursively.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_LIST_FILES_RECURSIVELY_H
#define ONECLIENT_MESSAGES_FUSE_LIST_FILES_RECURSIVELY_H

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
 * The ListFiles class represents a FUSE request for file children.
 */
class ListFilesRecursively : public FileRequest {
public:
    /**
     * @copydoc GetFileChildrenAttrs(std::string)
     * @param offset A number of skipped entries at the beginning of directory's
     * children list.
     * @param size A number of returned entries of directory's children list.
     * @param indexToken Token which can be used by the server to find last
     * returned item.
     */
    ListFilesRecursively(const folly::fbstring &uuid, const std::size_t limit,
        folly::Optional<folly::fbstring> token,
        folly::Optional<folly::fbstring> startAfter,
        folly::Optional<folly::fbstring> prefix,
        std::vector<std::string> xattrs = {}, bool includeDirectories = true);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    const std::size_t m_size;
    folly::Optional<folly::fbstring> m_token;
    folly::Optional<folly::fbstring> m_startAfter;
    folly::Optional<folly::fbstring> m_prefix;
    std::vector<std::string> m_xattrs;
    bool m_includeDirectories;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_LIST_FILES_RECURSIVELY_H
