/**
 * @file fileList.h
 * @author Bartek Kryza
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_FILE_LIST_H
#define ONECLIENT_MESSAGES_FUSE_FILE_LIST_H

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
 * The FileList class represents server-sent file children list with
 * attributes.
 */
class FileList : public FuseResponse {
public:
    /**
     * Constructor.
     * @param serverMessage Protocol Buffers message representing
     * @c FileList counterpart.
     */
    FileList(std::unique_ptr<ProtocolServerMessage> serverMessage);

    /**
     * @return A list of directory's children, specified by their UUID and
     * filename.
     */
    const folly::fbvector<FileAttr> &files() const { return m_files; }

    /**
     * @return Optional index token which contains id of the last returned
     *         item.
     */
    folly::Optional<folly::fbstring> nextPageToken() const
    {
        return m_nextPageToken;
    }

    /**
     * @return Optional flag determining whether there are any more children
     *                  in directory.
     */
    folly::Optional<bool> isLast() const { return m_isLast; }

    std::string toString() const override;

private:
    folly::fbvector<FileAttr> m_files;

    folly::Optional<folly::fbstring> m_nextPageToken;

    folly::Optional<bool> m_isLast;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_FILE_LIST_H
