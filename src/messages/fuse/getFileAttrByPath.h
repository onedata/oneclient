/**
 * @file getFileAttrByPath.h
 * @author Bartek Kryza
 * @copyright (C) 2022 ACK CYFRONET AGH
 * @copyright This software is released under the MIT license cited in
 * 'LICENSE.txt'
 */

#ifndef ONECLIENT_MESSAGES_FUSE_GET_FILE_ATTR_BY_PATH_H
#define ONECLIENT_MESSAGES_FUSE_GET_FILE_ATTR_BY_PATH_H

#include "fileRequest.h"

#include <folly/FBString.h>
#include <folly/FBVector.h>
#include <folly/Optional.h>

#include <string>

namespace one {
namespace messages {
namespace fuse {

/**
 * The GetFileAttrByPath class represents a FUSE request for file attributes.
 */
class GetFileAttrByPath : public FileRequest {
public:
    /**
     * Constructor.
     * @param uuid UUID of the file for which attributes are requested.
     * @param path
     * @param xattrs
     */
    GetFileAttrByPath(const folly::fbstring &uuid, folly::fbstring path,
        folly::fbvector<folly::fbstring> xattrs);

    std::string toString() const override;

private:
    std::unique_ptr<ProtocolClientMessage> serializeAndDestroy() override;

    folly::fbstring m_path;
    folly::fbvector<folly::fbstring> m_xattrs;
};

} // namespace fuse
} // namespace messages
} // namespace one

#endif // ONECLIENT_MESSAGES_FUSE_GET_FILE_ATTR_BY_PATH_H
